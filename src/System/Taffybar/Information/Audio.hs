{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------------- 
-- |
-- Module      : System.Taffybar.Information.Audio
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Basic audio information using PulseAudio's DBus interface.
--
-----------------------------------------------------------------------------

module System.Taffybar.Information.Audio
  ( AudioInfo(..)
  , getAudioInfo
  , toggleAudioMute
  , adjustAudioVolume
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, bracket, try)
import Control.Monad (guard, join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Word (Word32)
import DBus
import DBus.Client
import DBus.Internal.Address ()
import DBus.Internal.Types (Serial(..))
import System.Environment (lookupEnv)
import System.Log.Logger (Priority(..))
import System.Taffybar.DBus.Client.Params
  ( paCoreInterfaceName
  , paCoreObjectPath
  , paDeviceInterfaceName
  , paServerLookupBusName
  , paServerLookupInterfaceName
  , paServerLookupObjectPath
  )
import System.Taffybar.Util (logPrintF, maybeToEither)

-- | Audio information for the default (or provided) sink.
data AudioInfo = AudioInfo
  { audioVolumePercent :: Maybe Int
  , audioMuted :: Maybe Bool
  , audioSinkName :: String
  } deriving (Eq, Show)

audioLogPath :: String
audioLogPath = "System.Taffybar.Information.Audio"

audioLogF :: (MonadIO m, Show t) => Priority -> String -> t -> m ()
audioLogF = logPrintF audioLogPath

-- | Query volume and mute state for the provided sink name.
-- Returns Nothing when neither value is available.
getAudioInfo :: String -> IO (Maybe AudioInfo)
getAudioInfo sinkSpec = do
  result <- withPulseAudio $ \client -> do
    sinkPath <- resolveSinkPath client sinkSpec
    maybe (return Nothing) (getDeviceInfo client) sinkPath
  return $ join result

-- | Toggle mute for the provided sink. Returns True on success.
toggleAudioMute :: String -> IO Bool
toggleAudioMute sinkSpec = do
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
adjustAudioVolume :: String -> Int -> IO Bool
adjustAudioVolume sinkSpec deltaPercent = do
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

withPulseAudio :: (Client -> IO a) -> IO (Maybe a)
withPulseAudio action = do
  addressString <- getPulseAudioAddress
  case addressString >>= parsePulseAddress of
    Nothing -> return Nothing
    Just addr -> do
      result <- try $ bracket (connect addr) disconnect action
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
        Left (_ :: SomeException) -> return Nothing
        Right client -> do
          pulseAddr <- getServerLookupAddress client
          disconnect client
          return pulseAddr

getServerLookupAddress :: Client -> IO (Maybe String)
getServerLookupAddress client = do
  propsResult <- getProperties client (Just paServerLookupBusName)
    paServerLookupPath paServerLookupInterfaceName
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
      let
        name = readDictMaybe props "Name" :: Maybe String
        description = readDictMaybe props "Description" :: Maybe String
      in return $ Just target == name || Just target == description

getDeviceInfo :: Client -> ObjectPath -> IO (Maybe AudioInfo)
getDeviceInfo client path = do
  propsResult <- getProperties client Nothing path paDeviceInterfaceName
  case propsResult of
    Left err -> do
      audioLogF WARNING "Failed to read device properties: %s" err
      return Nothing
    Right props -> return $ Just $ buildAudioInfo props

buildAudioInfo :: Map Text Variant -> AudioInfo
buildAudioInfo props =
  let
    volumePercent = calcVolumePercent props
    muted = readDictMaybe props "Mute"
    name = fromMaybe "" (readDictMaybe props "Description" <|> readDictMaybe props "Name")
  in AudioInfo
    { audioVolumePercent = volumePercent
    , audioMuted = muted
    , audioSinkName = name
    }

calcVolumePercent :: Map Text Variant -> Maybe Int
calcVolumePercent props = do
  volumes <- readDictMaybe props "Volume" :: Maybe [Word32]
  guard (not (null volumes))
  let maxVolume = fromMaybe defaultMaxVolume (readDictMaybe props "MaxVolume")
  guard (maxVolume > 0)
  let
    total = sum volumes
    avg = fromIntegral total / fromIntegral (length volumes) :: Double
    percent = round $ avg * 100 / fromIntegral maxVolume
  return percent

defaultMaxVolume :: Word32
defaultMaxVolume = 65536

setDeviceMute :: Client -> ObjectPath -> Bool -> IO Bool
setDeviceMute client path muteState = do
  let muteCall = (methodCall path paDeviceInterfaceName "Mute")
        { methodCallDestination = Nothing }
  err <- setPropertyValue client muteCall muteState
  return $ err == Nothing

setDeviceVolume :: Client -> ObjectPath -> Word32 -> [Word32] -> Int -> IO Bool
setDeviceVolume client path maxVol volumes deltaPercent = do
  let maxI = fromIntegral maxVol :: Int
      delta = round (fromIntegral maxI * fromIntegral deltaPercent / 100.0 :: Double)
      newVolumes = map (fromIntegral . clamp 0 maxI . (+ delta) . fromIntegral) volumes :: [Word32]
      volCall = (methodCall path paDeviceInterfaceName "Volume")
        { methodCallDestination = Nothing }
  err <- setPropertyValue client volCall newVolumes
  return $ err == Nothing

clamp :: Int -> Int -> Int -> Int
clamp low high value = max low (min high value)

isObjectPath :: String -> Bool
isObjectPath ('/':_) = True
isObjectPath _ = False

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM predicate (x:xs) = do
  matches <- predicate x
  if matches
    then return (Just x)
    else findM predicate xs

readDictMaybe :: IsVariant a => Map Text Variant -> Text -> Maybe a
readDictMaybe dict key = M.lookup key dict >>= fromVariant

getProperties
  :: Client
  -> Maybe BusName
  -> ObjectPath
  -> InterfaceName
  -> IO (Either MethodError (Map Text Variant))
getProperties client destination path iface = runExceptT $ do
  reply <- ExceptT $ getAllProperties client $
    (methodCall path iface "FakeMethod")
      { methodCallDestination = destination }
  dict <- ExceptT $ return $ maybeToEither dummyMethodError $
    listToMaybe (methodReturnBody reply) >>= fromVariant
  return dict

-- XXX: Remove this once it is exposed in haskell-dbus
-- Same workaround pattern used elsewhere in Taffybar.
dummyMethodError :: MethodError
dummyMethodError = methodError (Serial 1) $ errorName_ "org.ClientTypeMismatch"
