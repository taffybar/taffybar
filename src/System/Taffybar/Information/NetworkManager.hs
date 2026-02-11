{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.NetworkManager
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides information about the active WiFi connection using
-- NetworkManager's DBus API.
module System.Taffybar.Information.NetworkManager
  ( WifiInfo (..),
    WifiState (..),
    NetworkInfo (..),
    NetworkState (..),
    NetworkType (..),
    getWifiInfo,
    getWifiInfoFromClient,
    getWifiInfoChan,
    getWifiInfoState,
    getNetworkInfo,
    getNetworkInfoFromClient,
    getNetworkInfoChan,
    getNetworkInfoState,
  )
where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import DBus
import DBus.Client
import DBus.Internal.Types (Serial (..))
import qualified DBus.TH as DBus
import qualified Data.ByteString as BS
import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Word (Word8)
import System.Log.Logger
import System.Taffybar.Context
import System.Taffybar.DBus.Client.Params
import System.Taffybar.Util (logPrintF, maybeToEither)

data WifiState
  = WifiDisabled
  | WifiDisconnected
  | WifiConnected
  | WifiUnknown
  deriving (Eq, Show)

data WifiInfo = WifiInfo
  { wifiState :: WifiState,
    wifiSsid :: Maybe Text,
    wifiStrength :: Maybe Int,
    wifiConnectionId :: Maybe Text
  }
  deriving (Eq, Show)

data NetworkState
  = NetworkConnected
  | NetworkDisconnected
  | NetworkUnknown
  deriving (Eq, Show)

data NetworkType
  = NetworkWifi
  | NetworkWired
  | NetworkVpn
  | NetworkOther Text
  deriving (Eq, Show)

data NetworkInfo = NetworkInfo
  { networkState :: NetworkState,
    networkType :: Maybe NetworkType,
    networkSsid :: Maybe Text,
    networkStrength :: Maybe Int,
    networkConnectionId :: Maybe Text,
    networkWirelessEnabled :: Maybe Bool
  }
  deriving (Eq, Show)

wifiLogPath :: String
wifiLogPath = "System.Taffybar.Information.NetworkManager"

wifiLogF :: (MonadIO m, Show t) => Priority -> String -> t -> m ()
wifiLogF = logPrintF wifiLogPath

nullObjectPath :: ObjectPath
nullObjectPath = objectPath_ "/"

wifiUnknownInfo :: WifiInfo
wifiUnknownInfo =
  WifiInfo
    { wifiState = WifiUnknown,
      wifiSsid = Nothing,
      wifiStrength = Nothing,
      wifiConnectionId = Nothing
    }

wifiDisabledInfo :: WifiInfo
wifiDisabledInfo =
  WifiInfo
    { wifiState = WifiDisabled,
      wifiSsid = Nothing,
      wifiStrength = Nothing,
      wifiConnectionId = Nothing
    }

wifiDisconnectedInfo :: WifiInfo
wifiDisconnectedInfo =
  WifiInfo
    { wifiState = WifiDisconnected,
      wifiSsid = Nothing,
      wifiStrength = Nothing,
      wifiConnectionId = Nothing
    }

newtype WifiInfoChanVar = WifiInfoChanVar (TChan WifiInfo, MVar WifiInfo)

getWifiInfoState :: TaffyIO WifiInfo
getWifiInfoState = do
  WifiInfoChanVar (_, theVar) <- getWifiInfoChanVar
  lift $ readMVar theVar

getWifiInfoChan :: TaffyIO (TChan WifiInfo)
getWifiInfoChan = do
  WifiInfoChanVar (chan, _) <- getWifiInfoChanVar
  return chan

getWifiInfoChanVar :: TaffyIO WifiInfoChanVar
getWifiInfoChanVar =
  getStateDefault $ WifiInfoChanVar <$> monitorWifiInfo

monitorWifiInfo :: TaffyIO (TChan WifiInfo, MVar WifiInfo)
monitorWifiInfo = do
  _client <- asks systemDBusClient
  infoVar <- lift $ newMVar wifiUnknownInfo
  chan <- liftIO newBroadcastTChanIO
  taffyFork $ do
    ctx <- ask
    let updateInfo = updateWifiInfo chan infoVar
        signalCallback _ _ _ _ = runReaderT updateInfo ctx
    _ <- registerForNetworkManagerPropertiesChanged signalCallback
    _ <- registerForActiveConnectionPropertiesChanged signalCallback
    _ <- registerForAccessPointPropertiesChanged signalCallback
    updateInfo
  return (chan, infoVar)

registerForNetworkManagerPropertiesChanged ::
  (Signal -> String -> Map String Variant -> [String] -> IO ()) ->
  ReaderT Context IO SignalHandler
registerForNetworkManagerPropertiesChanged signalHandler = do
  client <- asks systemDBusClient
  lift $
    DBus.registerForPropertiesChanged
      client
      matchAny
        { matchInterface = Just nmInterfaceName,
          matchPath = Just nmObjectPath
        }
      signalHandler

registerForActiveConnectionPropertiesChanged ::
  (Signal -> String -> Map String Variant -> [String] -> IO ()) ->
  ReaderT Context IO SignalHandler
registerForActiveConnectionPropertiesChanged signalHandler = do
  client <- asks systemDBusClient
  lift $
    DBus.registerForPropertiesChanged
      client
      matchAny
        { matchInterface = Just nmActiveConnectionInterfaceName,
          matchPathNamespace = Just nmActiveConnectionPathNamespace
        }
      signalHandler

registerForAccessPointPropertiesChanged ::
  (Signal -> String -> Map String Variant -> [String] -> IO ()) ->
  ReaderT Context IO SignalHandler
registerForAccessPointPropertiesChanged signalHandler = do
  client <- asks systemDBusClient
  lift $
    DBus.registerForPropertiesChanged
      client
      matchAny
        { matchInterface = Just nmAccessPointInterfaceName,
          matchPathNamespace = Just nmAccessPointPathNamespace
        }
      signalHandler

updateWifiInfo ::
  TChan WifiInfo ->
  MVar WifiInfo ->
  TaffyIO ()
updateWifiInfo chan var = do
  info <- getWifiInfo
  lift $ do
    _ <- swapMVar var info
    atomically $ writeTChan chan info

-- XXX: Remove this once it is exposed in haskell-dbus
dummyMethodError :: MethodError
dummyMethodError = methodError (Serial 1) $ errorName_ "org.ClientTypeMismatch"

readDictMaybe :: (IsVariant a) => Map Text Variant -> Text -> Maybe a
readDictMaybe dict key = M.lookup key dict >>= fromVariant

getProperties ::
  Client ->
  ObjectPath ->
  InterfaceName ->
  IO (Either MethodError (Map Text Variant))
getProperties client path iface = runExceptT $ do
  reply <-
    ExceptT $
      getAllProperties client $
        (methodCall path iface "FakeMethod")
          { methodCallDestination = Just nmBusName
          }
  ExceptT $
    return $
      maybeToEither dummyMethodError $
        listToMaybe (methodReturnBody reply) >>= fromVariant

getWifiInfo :: TaffyIO WifiInfo
getWifiInfo = asks systemDBusClient >>= liftIO . getWifiInfoFromClient

getWifiInfoFromClient :: Client -> IO WifiInfo
getWifiInfoFromClient client = do
  nmPropsResult <- getProperties client nmObjectPath nmInterfaceName
  case nmPropsResult of
    Left err -> do
      wifiLogF WARNING "Failed to read NetworkManager properties: %s" err
      return wifiUnknownInfo
    Right nmProps -> do
      let wirelessEnabled = readDictMaybe nmProps "WirelessEnabled" :: Maybe Bool
          activeConnections =
            readDictMaybe nmProps "ActiveConnections" :: Maybe [ObjectPath]
      case wirelessEnabled of
        Just False -> return wifiDisabledInfo
        Just True -> case activeConnections of
          Just paths ->
            fromMaybe wifiDisconnectedInfo
              <$> findActiveWifi client paths
          Nothing -> do
            logM wifiLogPath WARNING "NetworkManager missing ActiveConnections"
            return wifiUnknownInfo
        Nothing -> do
          logM wifiLogPath WARNING "NetworkManager missing WirelessEnabled"
          return wifiUnknownInfo

findActiveWifi :: Client -> [ObjectPath] -> IO (Maybe WifiInfo)
findActiveWifi _ [] = return Nothing
findActiveWifi client (path : rest) = do
  connPropsResult <- getProperties client path nmActiveConnectionInterfaceName
  case connPropsResult of
    Left err -> do
      wifiLogF DEBUG "Failed to read active connection %s" err
      findActiveWifi client rest
    Right connProps -> do
      let connType = readDictMaybe connProps "Type" :: Maybe Text
      if connType /= Just "802-11-wireless"
        then findActiveWifi client rest
        else do
          let connId = readDictMaybe connProps "Id" :: Maybe Text
              specificObject =
                readDictMaybe connProps "SpecificObject" :: Maybe ObjectPath
          (ssid, strength) <- getAccessPointInfo client specificObject
          return $
            Just
              WifiInfo
                { wifiState = WifiConnected,
                  wifiSsid = ssid,
                  wifiStrength = strength,
                  wifiConnectionId = connId
                }

getAccessPointInfo ::
  Client ->
  Maybe ObjectPath ->
  IO (Maybe Text, Maybe Int)
getAccessPointInfo _ Nothing = return (Nothing, Nothing)
getAccessPointInfo _ (Just path)
  | path == nullObjectPath =
      return (Nothing, Nothing)
getAccessPointInfo client (Just path) = do
  apPropsResult <- getProperties client path nmAccessPointInterfaceName
  case apPropsResult of
    Left err -> do
      wifiLogF DEBUG "Failed to read access point properties %s" err
      return (Nothing, Nothing)
    Right apProps -> do
      let ssidBytes = readDictMaybe apProps "Ssid" :: Maybe [Word8]
          strength = readDictMaybe apProps "Strength" :: Maybe Word8
      return
        ( ssidBytes >>= decodeSsid,
          fromIntegral <$> strength
        )

decodeSsid :: [Word8] -> Maybe Text
decodeSsid bytes
  | null bytes = Nothing
  | otherwise = Just $ TE.decodeUtf8With TEE.lenientDecode (BS.pack bytes)

-- Network info (WiFi + wired + VPN + disconnected)

newtype NetworkInfoChanVar = NetworkInfoChanVar (TChan NetworkInfo, MVar NetworkInfo)

networkUnknownInfo :: NetworkInfo
networkUnknownInfo =
  NetworkInfo
    { networkState = NetworkUnknown,
      networkType = Nothing,
      networkSsid = Nothing,
      networkStrength = Nothing,
      networkConnectionId = Nothing,
      networkWirelessEnabled = Nothing
    }

getNetworkInfoState :: TaffyIO NetworkInfo
getNetworkInfoState = do
  NetworkInfoChanVar (_, theVar) <- getNetworkInfoChanVar
  lift $ readMVar theVar

getNetworkInfoChan :: TaffyIO (TChan NetworkInfo)
getNetworkInfoChan = do
  NetworkInfoChanVar (chan, _) <- getNetworkInfoChanVar
  return chan

getNetworkInfoChanVar :: TaffyIO NetworkInfoChanVar
getNetworkInfoChanVar =
  getStateDefault $ NetworkInfoChanVar <$> monitorNetworkInfo

monitorNetworkInfo :: TaffyIO (TChan NetworkInfo, MVar NetworkInfo)
monitorNetworkInfo = do
  infoVar <- lift $ newMVar networkUnknownInfo
  chan <- liftIO newBroadcastTChanIO
  taffyFork $ do
    ctx <- ask
    let updateInfo = updateNetworkInfo chan infoVar
        signalCallback _ _ _ _ = runReaderT updateInfo ctx
    _ <- registerForNetworkManagerPropertiesChanged signalCallback
    _ <- registerForActiveConnectionPropertiesChanged signalCallback
    _ <- registerForAccessPointPropertiesChanged signalCallback
    updateInfo
  return (chan, infoVar)

updateNetworkInfo ::
  TChan NetworkInfo ->
  MVar NetworkInfo ->
  TaffyIO ()
updateNetworkInfo chan var = do
  info <- getNetworkInfo
  lift $ do
    _ <- swapMVar var info
    atomically $ writeTChan chan info

getNetworkInfo :: TaffyIO NetworkInfo
getNetworkInfo = asks systemDBusClient >>= liftIO . getNetworkInfoFromClient

data ActiveConnectionInfo = ActiveConnectionInfo
  { activeConnectionPath :: ObjectPath,
    activeConnectionType :: Text,
    activeConnectionId :: Maybe Text,
    activeConnectionSpecificObject :: Maybe ObjectPath
  }
  deriving (Eq, Show)

getNetworkInfoFromClient :: Client -> IO NetworkInfo
getNetworkInfoFromClient client = do
  nmPropsResult <- getProperties client nmObjectPath nmInterfaceName
  case nmPropsResult of
    Left err -> do
      wifiLogF WARNING "Failed to read NetworkManager properties: %s" err
      return networkUnknownInfo
    Right nmProps -> do
      let wirelessEnabled = readDictMaybe nmProps "WirelessEnabled" :: Maybe Bool
          activeConnections =
            readDictMaybe nmProps "ActiveConnections" :: Maybe [ObjectPath]

      activeInfos <- maybe (return []) (mapM (getActiveConnectionInfo client)) activeConnections
      let best = pickBestActiveConnection activeInfos

      case best of
        Nothing ->
          return
            networkUnknownInfo
              { networkState = NetworkDisconnected,
                networkWirelessEnabled = wirelessEnabled
              }
        Just ac -> do
          (ssid, strength) <-
            if activeConnectionType ac == "802-11-wireless"
              then getAccessPointInfo client (activeConnectionSpecificObject ac)
              else return (Nothing, Nothing)
          return
            networkUnknownInfo
              { networkState = NetworkConnected,
                networkType = Just $ toNetworkType (activeConnectionType ac),
                networkSsid = ssid,
                networkStrength = strength,
                networkConnectionId = activeConnectionId ac,
                networkWirelessEnabled = wirelessEnabled
              }

getActiveConnectionInfo :: Client -> ObjectPath -> IO ActiveConnectionInfo
getActiveConnectionInfo client path = do
  connPropsResult <- getProperties client path nmActiveConnectionInterfaceName
  case connPropsResult of
    Left err -> do
      wifiLogF DEBUG "Failed to read active connection %s" err
      return
        ActiveConnectionInfo
          { activeConnectionPath = path,
            activeConnectionType = "",
            activeConnectionId = Nothing,
            activeConnectionSpecificObject = Nothing
          }
    Right connProps -> do
      let connType = fromMaybe "" (readDictMaybe connProps "Type" :: Maybe Text)
          connId = readDictMaybe connProps "Id" :: Maybe Text
          specificObject =
            readDictMaybe connProps "SpecificObject" :: Maybe ObjectPath
      return
        ActiveConnectionInfo
          { activeConnectionPath = path,
            activeConnectionType = connType,
            activeConnectionId = connId,
            activeConnectionSpecificObject = specificObject
          }

pickBestActiveConnection :: [ActiveConnectionInfo] -> Maybe ActiveConnectionInfo
pickBestActiveConnection [] = Nothing
pickBestActiveConnection infos =
  let rank t
        | t == "802-3-ethernet" = 0 :: Int
        | t == "802-11-wireless" = 1
        | t == "vpn" = 2
        | T.null t = 99
        | otherwise = 50
   in Just $
        minimumBy
          (\a b -> compare (rank (activeConnectionType a)) (rank (activeConnectionType b)))
          infos

toNetworkType :: Text -> NetworkType
toNetworkType t
  | t == "802-11-wireless" = NetworkWifi
  | t == "802-3-ethernet" = NetworkWired
  | t == "vpn" = NetworkVpn
  | otherwise = NetworkOther t
