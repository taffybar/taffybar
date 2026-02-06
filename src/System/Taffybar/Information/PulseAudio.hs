{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.PulseAudio
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Basic PulseAudio information using PulseAudio's DBus interface.
--
-- Note: PulseAudio's DBus socket is not always enabled by default.
-- If callers see 'Nothing' from 'connectPulseAudio' (or 'getPulseAudioInfo'
-- returns 'Nothing'), ensure the PulseAudio server has loaded
-- @module-dbus-protocol@
-- so @/run/user/$UID/pulse/dbus-socket@ exists.
module System.Taffybar.Information.PulseAudio
  ( PulseAudioInfo (..),
    getPulseAudioInfo,
    getPulseAudioInfoFromClient,
    getPulseAudioInfoChan,
    getPulseAudioInfoState,
    connectPulseAudio,
    togglePulseAudioMute,
    adjustPulseAudioVolume,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception (SomeException, finally, throwIO, try)
import Control.Monad (forever, guard, join, void, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Except
import DBus
import DBus.Client
import DBus.Internal.Types (Serial (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Word (Word32)
import System.Directory (doesPathExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Log.Logger (Priority (..))
import System.Taffybar.DBus.Client.Params
  ( paCoreInterfaceName,
    paCoreObjectPath,
    paDeviceInterfaceName,
    paServerLookupBusName,
    paServerLookupInterfaceName,
    paServerLookupObjectPath,
  )
import System.Taffybar.Util (logPrintF, maybeToEither)

-- | PulseAudio information for the default (or provided) sink.
data PulseAudioInfo = PulseAudioInfo
  { pulseAudioVolumePercent :: Maybe Int,
    pulseAudioMuted :: Maybe Bool,
    pulseAudioSinkName :: String
  }
  deriving (Eq, Show)

audioLogPath :: String
audioLogPath = "System.Taffybar.Information.PulseAudio"

audioLogF :: (MonadIO m, Show t) => Priority -> String -> t -> m ()
audioLogF = logPrintF audioLogPath

newtype PulseAudioInfoChanVar
  = PulseAudioInfoChanVar (TChan (Maybe PulseAudioInfo), MVar (Maybe PulseAudioInfo))

{-# NOINLINE pulseAudioInfoChanVars #-}
pulseAudioInfoChanVars :: MVar (M.Map String PulseAudioInfoChanVar)
pulseAudioInfoChanVars = unsafePerformIO $ newMVar M.empty

-- | Get a broadcast channel for PulseAudio info for the provided sink spec.
--
-- The first call for a given sink spec will start a monitoring thread that
-- keeps a PulseAudio DBus connection open and refreshes on property changes.
-- Subsequent calls return the already created channel.
getPulseAudioInfoChan :: String -> IO (TChan (Maybe PulseAudioInfo))
getPulseAudioInfoChan sinkSpec = do
  PulseAudioInfoChanVar (chan, _) <- getPulseAudioInfoChanVar sinkSpec
  pure chan

-- | Read the current PulseAudio info state for the provided sink spec.
--
-- See 'getPulseAudioInfoChan' for monitoring behavior.
getPulseAudioInfoState :: String -> IO (Maybe PulseAudioInfo)
getPulseAudioInfoState sinkSpec = do
  PulseAudioInfoChanVar (_, var) <- getPulseAudioInfoChanVar sinkSpec
  readMVar var

getPulseAudioInfoChanVar :: String -> IO PulseAudioInfoChanVar
getPulseAudioInfoChanVar sinkSpec = do
  modifyMVar pulseAudioInfoChanVars $ \m ->
    case M.lookup sinkSpec m of
      Just existing -> pure (m, existing)
      Nothing -> do
        chan <- newBroadcastTChanIO
        var <- newMVar Nothing
        let chanVar = PulseAudioInfoChanVar (chan, var)
        _ <- forkIO $ monitorPulseAudioInfo sinkSpec chan var
        pure (M.insert sinkSpec chanVar m, chanVar)

monitorPulseAudioInfo ::
  String ->
  TChan (Maybe PulseAudioInfo) ->
  MVar (Maybe PulseAudioInfo) ->
  IO ()
monitorPulseAudioInfo sinkSpec chan var = do
  refreshLock <- newMVar ()
  let writeInfo info = do
        _ <- swapMVar var info
        atomically $ writeTChan chan info

      refreshWithClientUnlocked client = do
        result <- try $ getPulseAudioInfoFromClient client sinkSpec
        case result of
          Left (e :: SomeException) -> do
            audioLogF WARNING "Audio refresh failed: %s" e
            writeInfo Nothing
            pure $ Left e
          Right info -> writeInfo info >> pure (Right info)

      refreshWithClient client = withMVar refreshLock $ \_ -> refreshWithClientUnlocked client

      -- PulseAudio's DBus interface is usually exposed via a peer-to-peer
      -- connection (unix:path=$XDG_RUNTIME_DIR/pulse/dbus-socket). In that mode
      -- there is no bus daemon, so org.freedesktop.DBus.AddMatch/RemoveMatch do
      -- not exist.
      --
      -- Instead, PulseAudio provides org.PulseAudio.Core1.ListenForSignal which
      -- must be called to enable emission of signals like:
      --   org.PulseAudio.Core1.Device.VolumeUpdated
      --   org.PulseAudio.Core1.Device.MuteUpdated
      --
      -- We still use haskell-dbus 'addMatch' to register local handlers. When
      -- AddMatch fails (peer-to-peer), we ignore the exception; the handler is
      -- registered locally before the failing bus call.

      isMissingAddMatch :: ClientError -> Bool
      isMissingAddMatch e =
        let msg = clientErrorMessage e
         in "AddMatch" `isInfixOf` msg
              && ("doesn't exist" `isInfixOf` msg || "UnknownMethod" `isInfixOf` msg)

      addMatchP2P :: Client -> MatchRule -> (Signal -> IO ()) -> IO (Maybe SignalHandler)
      addMatchP2P client rule callback = do
        result <- try (addMatch client rule callback)
        case result of
          Right handler -> pure (Just handler)
          Left (e :: ClientError)
            | isMissingAddMatch e -> pure Nothing
            | otherwise -> throwIO e

      listenForSignal :: Client -> String -> [ObjectPath] -> IO ()
      listenForSignal client signalName objects = do
        let callMsg =
              (methodCall paCorePath paCoreInterfaceName "ListenForSignal")
                { methodCallDestination = Nothing
                , methodCallBody = [toVariant signalName, toVariant objects]
                }
        reply <- call client callMsg
        case reply of
          Left err -> audioLogF WARNING "PulseAudio ListenForSignal failed: %s" err
          Right _ -> pure ()

      paCoreInterfaceNameStr :: String
      paCoreInterfaceNameStr = "org.PulseAudio.Core1"

      paDeviceInterfaceNameStr :: String
      paDeviceInterfaceNameStr = "org.PulseAudio.Core1.Device"

      volumeUpdatedSignal :: String
      volumeUpdatedSignal = paDeviceInterfaceNameStr ++ ".VolumeUpdated"

      muteUpdatedSignal :: String
      muteUpdatedSignal = paDeviceInterfaceNameStr ++ ".MuteUpdated"

      coreSignalName :: String -> String
      coreSignalName memberName = paCoreInterfaceNameStr ++ "." ++ memberName

      coreFallbackUpdatedMember :: MemberName
      coreFallbackUpdatedMember = "FallbackSinkUpdated"

      coreFallbackUnsetMember :: MemberName
      coreFallbackUnsetMember = "FallbackSinkUnset"

      coreNewSinkMember :: MemberName
      coreNewSinkMember = "NewSink"

      coreSinkRemovedMember :: MemberName
      coreSinkRemovedMember = "SinkRemoved"

      deviceSignalMatcher :: MemberName -> MatchRule
      deviceSignalMatcher memberName =
        matchAny
          { matchPathNamespace = Just paCoreObjectPath
          , matchInterface = Just paDeviceInterfaceName
          , matchMember = Just memberName
          }

      coreSignalMatcher :: MemberName -> MatchRule
      coreSignalMatcher memberName =
        matchAny
          { matchPath = Just paCoreObjectPath
          , matchInterface = Just paCoreInterfaceName
          , matchMember = Just memberName
          }

      loop = do
        mClient <- connectPulseAudio
        case mClient of
          Nothing -> do
            writeInfo Nothing
            -- No polling; just retry connection periodically.
            threadDelay 5000000
            loop
          Just client -> do
            let runWithClient = do
                  subscribedSinksRef <- newIORef ([] :: [ObjectPath])

                  let getSinksUnlocked = do
                        corePropsResult <- getProperties client Nothing paCorePath paCoreInterfaceName
                        case corePropsResult of
                          Left err -> do
                            audioLogF WARNING "Failed to read PulseAudio Core1 properties: %s" err
                            pure []
                          Right coreProps ->
                            pure $ fromMaybe [] (readDictMaybe coreProps "Sinks")

                      updateDeviceSignalSubscriptionsUnlocked = do
                        sinks <- getSinksUnlocked
                        prev <- readIORef subscribedSinksRef
                        when (sinks /= prev && not (null sinks)) $ do
                          writeIORef subscribedSinksRef sinks
                          listenForSignal client volumeUpdatedSignal sinks
                          listenForSignal client muteUpdatedSignal sinks

                      resubscribeAndRefresh = withMVar refreshLock $ \_ -> do
                        updateDeviceSignalSubscriptionsUnlocked
                        void $ refreshWithClientUnlocked client

                  -- Ask PulseAudio to emit the signals we care about.
                  listenForSignal client (coreSignalName "FallbackSinkUpdated") [paCorePath]
                  listenForSignal client (coreSignalName "FallbackSinkUnset") [paCorePath]
                  listenForSignal client (coreSignalName "NewSink") [paCorePath]
                  listenForSignal client (coreSignalName "SinkRemoved") [paCorePath]

                  -- Emit device signals for the current set of sinks.
                  resubscribeAndRefresh

                  -- Install signal handlers. In peer-to-peer mode this will
                  -- throw a ClientError due to missing AddMatch, but the handler
                  -- remains registered locally and still works.
                  hVol <- addMatchP2P client (deviceSignalMatcher "VolumeUpdated") $ const resubscribeAndRefresh
                  hMute <- addMatchP2P client (deviceSignalMatcher "MuteUpdated") $ const resubscribeAndRefresh
                  hFallbackUpdated <- addMatchP2P client (coreSignalMatcher coreFallbackUpdatedMember) $ const resubscribeAndRefresh
                  hFallbackUnset <- addMatchP2P client (coreSignalMatcher coreFallbackUnsetMember) $ const resubscribeAndRefresh
                  hNewSink <- addMatchP2P client (coreSignalMatcher coreNewSinkMember) $ const resubscribeAndRefresh
                  hSinkRemoved <- addMatchP2P client (coreSignalMatcher coreSinkRemovedMember) $ const resubscribeAndRefresh

                  let cleanup = do
                        let removeIfJust = maybe (pure ()) (removeMatch client)
                        removeIfJust hVol
                        removeIfJust hMute
                        removeIfJust hFallbackUpdated
                        removeIfJust hFallbackUnset
                        removeIfJust hNewSink
                        removeIfJust hSinkRemoved

                  blockForever `finally` cleanup
            result <- try runWithClient
            case result of
              Left (e :: SomeException) ->
                audioLogF WARNING "Audio monitor error: %s" e
              Right _ -> pure ()
            disconnect client
            loop

      -- Avoid BlockedIndefinitelyOnMVar exceptions from the "empty mvar trick".
      blockForever =
        forever $ threadDelay 1000000000 -- ~1000s; interruptible by async exceptions.

  loop

-- | Query volume and mute state for the provided sink name.
-- Returns Nothing when neither value is available.
getPulseAudioInfo :: String -> IO (Maybe PulseAudioInfo)
getPulseAudioInfo sinkSpec = do
  result <- withPulseAudio (`getPulseAudioInfoFromClient` sinkSpec)
  return $ join result

getPulseAudioInfoFromClient :: Client -> String -> IO (Maybe PulseAudioInfo)
getPulseAudioInfoFromClient client sinkSpec = do
  sinkPath <- resolveSinkPath client sinkSpec
  maybe (return Nothing) (getDeviceInfo client) sinkPath

-- | Toggle mute for the provided sink. Returns True on success.
togglePulseAudioMute :: String -> IO Bool
togglePulseAudioMute sinkSpec = do
  result <- withPulseAudio $ \client -> do
    sinkPath <- resolveSinkPath client sinkSpec
    case sinkPath of
      Nothing -> return False
      Just path -> do
        propsResult <- getProperties client Nothing path paDeviceInterfaceName
        case propsResult of
          Left err -> do
            audioLogF WARNING "Failed to read device properties: %s" err
            return False
          Right props -> case readDictMaybe props "Mute" of
            Nothing -> return False
            Just muted -> setDeviceMute client path (not muted)
  return $ fromMaybe False result

-- | Adjust volume by the provided percentage delta. Returns True on success.
adjustPulseAudioVolume :: String -> Int -> IO Bool
adjustPulseAudioVolume sinkSpec deltaPercent = do
  result <- withPulseAudio $ \client -> do
    sinkPath <- resolveSinkPath client sinkSpec
    case sinkPath of
      Nothing -> return False
      Just path -> do
        propsResult <- getProperties client Nothing path paDeviceInterfaceName
        case propsResult of
          Left err -> do
            audioLogF WARNING "Failed to read device properties: %s" err
            return False
          Right props -> do
            let maxVol = fromMaybe defaultMaxVolume (readDictMaybe props "MaxVolume")
            case readDictMaybe props "Volume" of
              Nothing -> return False
              Just volumes -> setDeviceVolume client path maxVol volumes deltaPercent
  return $ fromMaybe False result

-- --------------------------------------------------------------------------
-- DBus helpers

paServerLookupPath :: ObjectPath
paServerLookupPath = paServerLookupObjectPath

paCorePath :: ObjectPath
paCorePath = paCoreObjectPath

nullObjectPath :: ObjectPath
nullObjectPath = objectPath_ "/"

connectPulseAudio :: IO (Maybe Client)
connectPulseAudio = do
  addressString <- getPulseAudioAddress
  case addressString >>= parsePulseAddress of
    Nothing -> return Nothing
    Just addr -> do
      result <- try (connect addr)
      case result of
        Left (_ :: SomeException) -> return Nothing
        Right client -> return (Just client)

withPulseAudio :: (Client -> IO a) -> IO (Maybe a)
withPulseAudio action = do
  mClient <- connectPulseAudio
  case mClient of
    Nothing -> return Nothing
    Just client -> do
      result <- try $ action client
      disconnect client
      case result of
        Left (_ :: SomeException) -> return Nothing
        Right value -> return $ Just value

parsePulseAddress :: String -> Maybe Address
parsePulseAddress addrStr = listToMaybe =<< parseAddresses addrStr

getPulseAudioAddress :: IO (Maybe String)
getPulseAudioAddress = do
  envAddress <- lookupEnv "PULSE_DBUS_SERVER"
  case envAddress of
    Just addr | not (null addr) -> return (Just addr)
    _ -> do
      result <- try connectSession
      case result of
        Left (_ :: SomeException) -> runtimeDirFallback
        Right client -> do
          pulseAddr <- getServerLookupAddress client
          disconnect client
          case pulseAddr of
            Just _ -> return pulseAddr
            Nothing -> runtimeDirFallback

-- Many setups (notably pipewire-pulse) have the peer-to-peer PulseAudio DBus
-- socket available at $XDG_RUNTIME_DIR/pulse/dbus-socket but do not expose a
-- ServerLookup1 entry on the session bus. Fall back to the well-known socket.
runtimeDirFallback :: IO (Maybe String)
runtimeDirFallback = do
  mRuntimeDir <- lookupEnv "XDG_RUNTIME_DIR"
  case mRuntimeDir of
    Nothing -> return Nothing
    Just runtimeDir -> do
      let sockPath = runtimeDir </> "pulse" </> "dbus-socket"
      exists <- doesPathExist sockPath
      return $
        if exists
          then Just ("unix:path=" ++ sockPath)
          else Nothing

getServerLookupAddress :: Client -> IO (Maybe String)
getServerLookupAddress client = do
  propsResult <-
    getProperties
      client
      (Just paServerLookupBusName)
      paServerLookupPath
      paServerLookupInterfaceName
  case propsResult of
    Left err -> do
      audioLogF WARNING "Failed to read PulseAudio ServerLookup1 properties: %s" err
      return Nothing
    Right props -> case readDictMaybe props "Address" of
      Just addr | not (null addr) -> return (Just addr)
      _ -> return Nothing

resolveSinkPath :: Client -> String -> IO (Maybe ObjectPath)
resolveSinkPath client sinkSpec = do
  corePropsResult <- getProperties client Nothing paCorePath paCoreInterfaceName
  case corePropsResult of
    Left err -> do
      audioLogF WARNING "Failed to read PulseAudio Core1 properties: %s" err
      return Nothing
    Right coreProps
      | sinkSpec == "@DEFAULT_SINK@" || null sinkSpec ->
          return $ selectDefaultSink coreProps
      | isObjectPath sinkSpec ->
          return $ Just $ objectPath_ sinkSpec
      | otherwise -> findSinkByName client coreProps sinkSpec

selectDefaultSink :: Map Text Variant -> Maybe ObjectPath
selectDefaultSink props =
  let fallback = readDictMaybe props "FallbackSink" :: Maybe ObjectPath
      sinks = readDictMaybe props "Sinks" :: Maybe [ObjectPath]
   in case fallback of
        Just path | path /= nullObjectPath -> Just path
        _ -> listToMaybe =<< sinks

findSinkByName :: Client -> Map Text Variant -> String -> IO (Maybe ObjectPath)
findSinkByName client props target =
  case readDictMaybe props "Sinks" :: Maybe [ObjectPath] of
    Nothing -> return Nothing
    Just sinks -> findM (sinkMatchesName client target) sinks

sinkMatchesName :: Client -> String -> ObjectPath -> IO Bool
sinkMatchesName client target path = do
  propsResult <- getProperties client Nothing path paDeviceInterfaceName
  case propsResult of
    Left _ -> return False
    Right props ->
      let name = readDictMaybe props "Name" :: Maybe String
          description = readDictMaybe props "Description" :: Maybe String
       in return $ Just target == name || Just target == description

getDeviceInfo :: Client -> ObjectPath -> IO (Maybe PulseAudioInfo)
getDeviceInfo client path = do
  propsResult <- getProperties client Nothing path paDeviceInterfaceName
  case propsResult of
    Left err -> do
      audioLogF WARNING "Failed to read device properties: %s" err
      return Nothing
    Right props -> return $ Just $ buildPulseAudioInfo props

buildPulseAudioInfo :: Map Text Variant -> PulseAudioInfo
buildPulseAudioInfo props =
  let volumePercent = calcVolumePercent props
      muted = readDictMaybe props "Mute"
      name = fromMaybe "" (readDictMaybe props "Description" <|> readDictMaybe props "Name")
   in PulseAudioInfo
        { pulseAudioVolumePercent = volumePercent,
          pulseAudioMuted = muted,
          pulseAudioSinkName = name
        }

calcVolumePercent :: Map Text Variant -> Maybe Int
calcVolumePercent props = do
  volumes <- readDictMaybe props "Volume" :: Maybe [Word32]
  guard (not (null volumes))
  let maxVolume = fromMaybe defaultMaxVolume (readDictMaybe props "MaxVolume")
  guard (maxVolume > 0)
  let total = sum volumes
      avg = fromIntegral total / fromIntegral (length volumes) :: Double
      percent = round $ avg * 100 / fromIntegral maxVolume
  return percent

defaultMaxVolume :: Word32
defaultMaxVolume = 65536

setDeviceMute :: Client -> ObjectPath -> Bool -> IO Bool
setDeviceMute client path muteState = do
  let muteCall =
        (methodCall path paDeviceInterfaceName "Mute")
          { methodCallDestination = Nothing
          }
  err <- setPropertyValue client muteCall muteState
  pure $ isNothing err

setDeviceVolume :: Client -> ObjectPath -> Word32 -> [Word32] -> Int -> IO Bool
setDeviceVolume client path maxVol volumes deltaPercent = do
  let maxI = fromIntegral maxVol :: Int
      delta = round (fromIntegral maxI * fromIntegral deltaPercent / 100.0 :: Double)
      newVolumes = map (fromIntegral . clamp 0 maxI . (+ delta) . fromIntegral) volumes :: [Word32]
      volCall =
        (methodCall path paDeviceInterfaceName "Volume")
          { methodCallDestination = Nothing
          }
  err <- setPropertyValue client volCall newVolumes
  pure $ isNothing err

clamp :: Int -> Int -> Int -> Int
clamp low high value = max low (min high value)

isObjectPath :: String -> Bool
isObjectPath ('/' : _) = True
isObjectPath _ = False

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM predicate (x : xs) = do
  matches <- predicate x
  if matches
    then return (Just x)
    else findM predicate xs

readDictMaybe :: (IsVariant a) => Map Text Variant -> Text -> Maybe a
readDictMaybe dict key = M.lookup key dict >>= fromVariant

getProperties ::
  Client ->
  Maybe BusName ->
  ObjectPath ->
  InterfaceName ->
  IO (Either MethodError (Map Text Variant))
getProperties client destination path iface = runExceptT $ do
  reply <-
    ExceptT $
      getAllProperties client $
        (methodCall path iface "FakeMethod")
          { methodCallDestination = destination
          }
  ExceptT $
    return $
      maybeToEither dummyMethodError $
        listToMaybe (methodReturnBody reply) >>= fromVariant

-- XXX: Remove this once it is exposed in haskell-dbus
-- Same workaround pattern used elsewhere in Taffybar.
dummyMethodError :: MethodError
dummyMethodError = methodError (Serial 1) $ errorName_ "org.ClientTypeMismatch"
