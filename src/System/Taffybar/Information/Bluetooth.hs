{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.Bluetooth
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides functions for querying Bluetooth information using the
-- BlueZ dbus interface (org.bluez), as well as a broadcast "TChan" system for
-- allowing multiple readers to receive 'BluetoothInfo' updates without
-- duplicating requests.
--
-- The module uses the DBus ObjectManager interface to dynamically discover
-- Bluetooth controllers and devices, and monitors property changes for
-- real-time updates.
module System.Taffybar.Information.Bluetooth
  ( -- * Data Types
    BluetoothInfo (..),
    BluetoothDevice (..),
    BluetoothController (..),
    BluetoothStatus (..),

    -- * Information Access
    getBluetoothInfo,
    getBluetoothInfoChan,
    getBluetoothInfoState,

    -- * Connection
    connectBluez,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception (SomeException, finally, try)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (atomically)
import DBus
import DBus.Client
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import System.Log.Logger (Priority (..))
import System.Taffybar.Context (TaffyIO, getStateDefault, getSystemDBusClient)
import System.Taffybar.Util (logPrintF)

-- | Information about a Bluetooth device.
data BluetoothDevice = BluetoothDevice
  { devicePath :: ObjectPath,
    deviceName :: String,
    deviceAlias :: String,
    deviceAddress :: String,
    deviceIcon :: Maybe String,
    deviceConnected :: Bool,
    devicePaired :: Bool,
    deviceTrusted :: Bool,
    deviceBlocked :: Bool,
    deviceBatteryPercentage :: Maybe Word8
  }
  deriving (Eq, Show)

-- | Information about a Bluetooth controller (adapter).
data BluetoothController = BluetoothController
  { controllerPath :: ObjectPath,
    controllerAlias :: String,
    controllerAddress :: String,
    controllerPowered :: Bool,
    controllerDiscoverable :: Bool,
    controllerDiscovering :: Bool,
    controllerPairable :: Bool
  }
  deriving (Eq, Show)

-- | Complete Bluetooth state information.
data BluetoothInfo = BluetoothInfo
  { bluetoothController :: Maybe BluetoothController,
    bluetoothConnectedDevices :: [BluetoothDevice],
    bluetoothAllDevices :: [BluetoothDevice],
    bluetoothStatus :: BluetoothStatus
  }
  deriving (Eq, Show)

-- | High-level Bluetooth status.
data BluetoothStatus
  = BluetoothNoController
  | BluetoothOff
  | BluetoothOn
  | BluetoothConnected
  deriving (Eq, Show)

bluetoothLogPath :: String
bluetoothLogPath = "System.Taffybar.Information.Bluetooth"

bluetoothLogF :: (MonadIO m, Show t) => Priority -> String -> t -> m ()
bluetoothLogF = logPrintF bluetoothLogPath

-- | BlueZ DBus constants
bluezBusName :: BusName
bluezBusName = "org.bluez"

bluezRootPath :: ObjectPath
bluezRootPath = "/"

objectManagerInterfaceName :: InterfaceName
objectManagerInterfaceName = "org.freedesktop.DBus.ObjectManager"

propertiesInterfaceName :: InterfaceName
propertiesInterfaceName = "org.freedesktop.DBus.Properties"

adapter1InterfaceName :: InterfaceName
adapter1InterfaceName = "org.bluez.Adapter1"

device1InterfaceName :: InterfaceName
device1InterfaceName = "org.bluez.Device1"

battery1InterfaceName :: InterfaceName
battery1InterfaceName = "org.bluez.Battery1"

-- | Newtype wrapper for the channel/mvar pair to enable getStateDefault.
newtype BluetoothInfoChanVar
  = BluetoothInfoChanVar (TChan BluetoothInfo, MVar BluetoothInfo)

-- | Get a broadcast channel for Bluetooth info updates.
--
-- The first call will start a monitoring thread that keeps the BlueZ DBus
-- connection open and refreshes on property changes. Subsequent calls return
-- the already created channel.
getBluetoothInfoChan :: TaffyIO (TChan BluetoothInfo)
getBluetoothInfoChan = do
  BluetoothInfoChanVar (chan, _) <- getBluetoothInfoChanVar
  pure chan

-- | Read the current Bluetooth info state.
getBluetoothInfoState :: TaffyIO BluetoothInfo
getBluetoothInfoState = do
  BluetoothInfoChanVar (_, var) <- getBluetoothInfoChanVar
  liftIO $ readMVar var

getBluetoothInfoChanVar :: TaffyIO BluetoothInfoChanVar
getBluetoothInfoChanVar =
  getStateDefault $ do
    client <- getSystemDBusClient
    liftIO $ do
      chan <- newBroadcastTChanIO
      var <- newMVar defaultBluetoothInfo
      _ <- forkIO $ monitorBluetoothInfo client chan var
      pure $ BluetoothInfoChanVar (chan, var)

defaultBluetoothInfo :: BluetoothInfo
defaultBluetoothInfo =
  BluetoothInfo
    { bluetoothController = Nothing,
      bluetoothConnectedDevices = [],
      bluetoothAllDevices = [],
      bluetoothStatus = BluetoothNoController
    }

-- | Monitor Bluetooth information changes.
monitorBluetoothInfo ::
  Client ->
  TChan BluetoothInfo ->
  MVar BluetoothInfo ->
  IO ()
monitorBluetoothInfo client chan var = do
  refreshLock <- newMVar ()
  let writeInfo info = do
        _ <- swapMVar var info
        atomically $ writeTChan chan info

      refreshUnlocked = do
        result <- try $ getBluetoothInfoFromClient client
        case result of
          Left (e :: SomeException) -> do
            bluetoothLogF WARNING "Bluetooth refresh failed: %s" e
            writeInfo defaultBluetoothInfo
          Right info -> writeInfo info

      refresh = withMVar refreshLock $ const refreshUnlocked

      -- Match rule for BlueZ property changes
      propertiesChangedMatcher :: MatchRule
      propertiesChangedMatcher =
        matchAny
          { matchSender = Just bluezBusName,
            matchInterface = Just propertiesInterfaceName,
            matchMember = Just "PropertiesChanged"
          }

      -- Match rule for ObjectManager signals
      interfacesAddedMatcher :: MatchRule
      interfacesAddedMatcher =
        matchAny
          { matchSender = Just bluezBusName,
            matchInterface = Just objectManagerInterfaceName,
            matchMember = Just "InterfacesAdded"
          }

      interfacesRemovedMatcher :: MatchRule
      interfacesRemovedMatcher =
        matchAny
          { matchSender = Just bluezBusName,
            matchInterface = Just objectManagerInterfaceName,
            matchMember = Just "InterfacesRemoved"
          }

      loop = do
        -- Initial refresh
        refresh

        let runWithClient = do
              -- Register signal handlers
              hProps <- addMatch client propertiesChangedMatcher (const refresh)
              hAdded <- addMatch client interfacesAddedMatcher (const refresh)
              hRemoved <- addMatch client interfacesRemovedMatcher (const refresh)

              let cleanup = do
                    removeMatch client hProps
                    removeMatch client hAdded
                    removeMatch client hRemoved

              -- Block forever until an exception occurs
              blockForever `finally` cleanup

        result <- try runWithClient
        case result of
          Left (e :: SomeException) ->
            bluetoothLogF WARNING "Bluetooth monitor error: %s" e
          Right _ -> pure ()

        -- Wait before retrying
        threadDelay 5000000
        loop

      blockForever = forever $ threadDelay 1000000000

  loop

-- | Get Bluetooth information from an existing DBus client.
getBluetoothInfoFromClient :: Client -> IO BluetoothInfo
getBluetoothInfoFromClient client = do
  managedObjects <- getManagedObjects client
  case managedObjects of
    Left err -> do
      bluetoothLogF WARNING "Failed to get BlueZ managed objects: %s" err
      return defaultBluetoothInfo
    Right objects -> do
      let controllers = parseControllers objects
          devices = parseDevices objects
          connectedDevices = filter deviceConnected devices
          controller = listToMaybe controllers
          status = case controller of
            Nothing -> BluetoothNoController
            Just c
              | not (controllerPowered c) -> BluetoothOff
              | not (null connectedDevices) -> BluetoothConnected
              | otherwise -> BluetoothOn
      return
        BluetoothInfo
          { bluetoothController = controller,
            bluetoothConnectedDevices = connectedDevices,
            bluetoothAllDevices = devices,
            bluetoothStatus = status
          }

-- | Get Bluetooth info using the system DBus client.
getBluetoothInfo :: Client -> IO BluetoothInfo
getBluetoothInfo = getBluetoothInfoFromClient

-- | Connect to BlueZ on the system bus.
connectBluez :: IO (Maybe Client)
connectBluez = do
  result <- try connectSystem
  case result of
    Left (_ :: SomeException) -> return Nothing
    Right client -> return (Just client)

-- | Get all managed objects from BlueZ ObjectManager.
getManagedObjects :: Client -> IO (Either MethodError (Map ObjectPath (Map Text (Map Text Variant))))
getManagedObjects client = do
  let callMsg =
        (methodCall bluezRootPath objectManagerInterfaceName "GetManagedObjects")
          { methodCallDestination = Just bluezBusName
          }
  reply <- call client callMsg
  return $ case reply of
    Left err -> Left err
    Right ret -> case listToMaybe (methodReturnBody ret) >>= fromVariant of
      Nothing ->
        Left $
          methodError (methodReturnSerial ret) $
            errorName_ "org.taffybar.InvalidResponse"
      Just objects -> Right objects

-- | Parse controllers from managed objects.
parseControllers :: Map ObjectPath (Map Text (Map Text Variant)) -> [BluetoothController]
parseControllers objects =
  sortOn controllerPath $ mapMaybe parseController $ M.toList objects
  where
    parseController :: (ObjectPath, Map Text (Map Text Variant)) -> Maybe BluetoothController
    parseController (path, interfaces) = do
      props <- M.lookup (T.pack $ formatInterfaceName adapter1InterfaceName) interfaces
      let readProp :: (IsVariant a) => Text -> a -> a
          readProp key def = fromMaybe def $ M.lookup key props >>= fromVariant
      return
        BluetoothController
          { controllerPath = path,
            controllerAlias = readProp "Alias" "",
            controllerAddress = readProp "Address" "",
            controllerPowered = readProp "Powered" False,
            controllerDiscoverable = readProp "Discoverable" False,
            controllerDiscovering = readProp "Discovering" False,
            controllerPairable = readProp "Pairable" False
          }

-- | Parse devices from managed objects.
parseDevices :: Map ObjectPath (Map Text (Map Text Variant)) -> [BluetoothDevice]
parseDevices objects =
  sortOn devicePath $ mapMaybe parseDevice $ M.toList objects
  where
    parseDevice :: (ObjectPath, Map Text (Map Text Variant)) -> Maybe BluetoothDevice
    parseDevice (path, interfaces) = do
      props <- M.lookup (T.pack $ formatInterfaceName device1InterfaceName) interfaces
      let readProp :: (IsVariant a) => Text -> a -> a
          readProp key def = fromMaybe def $ M.lookup key props >>= fromVariant
          batteryProps = M.lookup (T.pack $ formatInterfaceName battery1InterfaceName) interfaces
          batteryPct = batteryProps >>= M.lookup "Percentage" >>= fromVariant
      return
        BluetoothDevice
          { devicePath = path,
            deviceName = readProp "Name" "",
            deviceAlias = readProp "Alias" "",
            deviceAddress = readProp "Address" "",
            deviceIcon = M.lookup "Icon" props >>= fromVariant,
            deviceConnected = readProp "Connected" False,
            devicePaired = readProp "Paired" False,
            deviceTrusted = readProp "Trusted" False,
            deviceBlocked = readProp "Blocked" False,
            deviceBatteryPercentage = batteryPct
          }
