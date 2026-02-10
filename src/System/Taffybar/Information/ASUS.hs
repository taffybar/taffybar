{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.ASUS
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides information about the current ASUS platform profile
-- and CPU state using the asusd DBus API (xyz.ljones.Asusd) and sysfs.
-----------------------------------------------------------------------------
module System.Taffybar.Information.ASUS
  ( ASUSPlatformProfile(..)
  , ASUSInfo(..)
  , getASUSInfo
  , getASUSInfoFromClient
  , getASUSInfoChan
  , getASUSInfoState
  , cycleASUSProfile
  , setASUSProfile
  , asusProfileToString
  , asusProfileFromUInt
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TChan
import           Control.Exception (SomeException, try)
import           Control.Monad (forM, forever, void)
import           Control.Monad.IO.Class
import           Control.Monad.STM (atomically)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           DBus
import           DBus.Client
import           DBus.Internal.Types (Serial(..))
import qualified DBus.TH as DBus
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import           Data.Text (Text)
import           Data.Word (Word32)
import           System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import           System.FilePath ((</>))
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.Util (logPrintF, maybeToEither)
import           Text.Read (readMaybe)

-- | ASUS platform profile modes.
data ASUSPlatformProfile = Quiet | Performance | Balanced
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Combined ASUS platform info with CPU state.
data ASUSInfo = ASUSInfo
  { asusProfile    :: ASUSPlatformProfile
  , asusCpuFreqGHz :: Double        -- ^ Average CPU frequency across all cores
  , asusCpuTempC   :: Double        -- ^ CPU package temperature in Celsius
  } deriving (Eq, Show)

-- DBus constants

asusBusName :: BusName
asusBusName = "xyz.ljones.Asusd"

asusObjectPath :: ObjectPath
asusObjectPath = "/xyz/ljones"

asusInterfaceName :: InterfaceName
asusInterfaceName = "xyz.ljones.Platform"

asusLogPath :: String
asusLogPath = "System.Taffybar.Information.ASUS"

asusLogF :: (MonadIO m, Show t) => Priority -> String -> t -> m ()
asusLogF = logPrintF asusLogPath

-- | Convert profile enum to string.
asusProfileToString :: ASUSPlatformProfile -> Text
asusProfileToString Quiet = "Quiet"
asusProfileToString Balanced = "Balanced"
asusProfileToString Performance = "Performance"

-- | Parse profile from asusd uint32: 0=Quiet, 1=Performance, 2=Balanced.
asusProfileFromUInt :: Word32 -> Maybe ASUSPlatformProfile
asusProfileFromUInt 0 = Just Quiet
asusProfileFromUInt 1 = Just Performance
asusProfileFromUInt 2 = Just Balanced
asusProfileFromUInt _ = Nothing

asusProfileToUInt :: ASUSPlatformProfile -> Word32
asusProfileToUInt Quiet = 0
asusProfileToUInt Performance = 1
asusProfileToUInt Balanced = 2

-- | Default info when asusd is unavailable.
unknownASUSInfo :: ASUSInfo
unknownASUSInfo = ASUSInfo
  { asusProfile = Balanced
  , asusCpuFreqGHz = 0
  , asusCpuTempC = 0
  }

-- XXX: Remove this once it is exposed in haskell-dbus
dummyMethodError :: MethodError
dummyMethodError = methodError (Serial 1) $ errorName_ "org.ClientTypeMismatch"

readDictMaybe :: IsVariant a => Map Text Variant -> Text -> Maybe a
readDictMaybe dict key = M.lookup key dict >>= fromVariant

getProperties
  :: Client
  -> IO (Either MethodError (Map Text Variant))
getProperties client = runExceptT $ do
  reply <- ExceptT $ getAllProperties client $
    (methodCall asusObjectPath asusInterfaceName "FakeMethod")
      { methodCallDestination = Just asusBusName }
  ExceptT $ return $ maybeToEither dummyMethodError $
    listToMaybe (methodReturnBody reply) >>= fromVariant

-- | Read current platform profile from DBus.
readProfileFromClient :: Client -> IO ASUSPlatformProfile
readProfileFromClient client = do
  propsResult <- getProperties client
  case propsResult of
    Left err -> do
      asusLogF WARNING "Failed to read ASUS properties: %s" err
      return Balanced
    Right props ->
      let profileVal = readDictMaybe props "PlatformProfile" :: Maybe Word32
      in return $ fromMaybe Balanced (profileVal >>= asusProfileFromUInt)

-- | Set the ASUS platform profile via DBus property.
setASUSProfile :: Client -> ASUSPlatformProfile -> IO (Either MethodError ())
setASUSProfile client profile = do
  result <- setProperty client
    (methodCall asusObjectPath asusInterfaceName "PlatformProfile")
      { methodCallDestination = Just asusBusName }
    (toVariant (asusProfileToUInt profile))
  return $ case result of
    Left err -> Left err
    Right _  -> Right ()

-- | Cycle to the next profile by calling the NextPlatformProfile method.
cycleASUSProfile :: Client -> IO (Either MethodError ())
cycleASUSProfile client = do
  let mc = (methodCall asusObjectPath asusInterfaceName "NextPlatformProfile")
             { methodCallDestination = Just asusBusName }
  result <- call client mc
  return $ case result of
    Left err -> Left err
    Right _  -> Right ()

-- sysfs CPU frequency reading

-- | Read average CPU frequency in GHz from sysfs.
readCpuFreqGHz :: IO Double
readCpuFreqGHz = do
  let cpuDir = "/sys/devices/system/cpu"
  exists <- doesDirectoryExist cpuDir
  if not exists
    then return 0
    else do
      entries <- listDirectory cpuDir
      let cpuDirs = filter isCpuDir entries
      freqs <- forM cpuDirs $ \cpu -> do
        let freqPath = cpuDir </> cpu </> "cpufreq" </> "scaling_cur_freq"
        readFreqFile freqPath
      let validFreqs = catMaybes freqs
      if null validFreqs
        then return 0
        else return $ (sum validFreqs / fromIntegral (length validFreqs)) / 1_000_000
  where
    isCpuDir name =
      take 3 name == "cpu" && all (`elem` ("0123456789" :: String)) (drop 3 name)
    readFreqFile path = do
      exists' <- doesFileExist path
      if not exists'
        then return Nothing
        else do
          result <- try $ readFile path :: IO (Either SomeException String)
          case result of
            Left _  -> return Nothing
            Right s -> return $ fmap fromIntegral (readMaybe (strip s) :: Maybe Integer)
    strip = dropWhile (== ' ') . reverse . dropWhile (== '\n') . reverse . dropWhile (== ' ')

-- sysfs CPU temperature reading

-- | Read CPU package temperature in Celsius from sysfs.
-- Prefers x86_pkg_temp zone, falls back to highest temperature.
readCpuTempC :: IO Double
readCpuTempC = do
  let thermalDir = "/sys/class/thermal"
  exists <- doesDirectoryExist thermalDir
  if not exists
    then return 0
    else do
      entries <- listDirectory thermalDir
      let zones = filter (\e -> take 12 e == "thermal_zone") entries
      readings <- forM zones $ \zone -> do
        let typePath = thermalDir </> zone </> "type"
            tempPath = thermalDir </> zone </> "temp"
        zoneType <- readFileSafe typePath
        tempVal <- readTempFile tempPath
        return $ case tempVal of
          Nothing -> Nothing
          Just t  -> Just (fromMaybe zone zoneType, t)
      let validReadings = catMaybes readings
          pkgTemp = lookup "x86_pkg_temp" validReadings
      case pkgTemp of
        Just t  -> return t
        Nothing ->
          if null validReadings
            then return 0
            else return $ maximum $ map snd validReadings
  where
    readFileSafe path = do
      exists' <- doesFileExist path
      if not exists'
        then return Nothing
        else do
          result <- try $ readFile path :: IO (Either SomeException String)
          case result of
            Left _  -> return Nothing
            Right s -> return $ Just $ strip s
    readTempFile path = do
      exists' <- doesFileExist path
      if not exists'
        then return Nothing
        else do
          result <- try $ readFile path :: IO (Either SomeException String)
          case result of
            Left _  -> return Nothing
            Right s -> case readMaybe (strip s) :: Maybe Integer of
              Nothing -> return Nothing
              Just milliDeg -> return $ Just (fromIntegral milliDeg / 1000.0)
    strip = dropWhile (== ' ') . reverse . dropWhile (== '\n') . reverse . dropWhile (== ' ')

-- | Get current ASUS info using the system DBus client from Context.
getASUSInfo :: TaffyIO ASUSInfo
getASUSInfo = asks systemDBusClient >>= liftIO . getASUSInfoFromClient

-- | Get current ASUS info from a DBus client.
getASUSInfoFromClient :: Client -> IO ASUSInfo
getASUSInfoFromClient client = do
  profile <- readProfileFromClient client
  freq <- readCpuFreqGHz
  temp <- readCpuTempC
  return ASUSInfo
    { asusProfile = profile
    , asusCpuFreqGHz = freq
    , asusCpuTempC = temp
    }

-- State management for monitoring

newtype ASUSInfoChanVar =
  ASUSInfoChanVar (TChan ASUSInfo, MVar ASUSInfo)

-- | Get the current ASUS info state.
getASUSInfoState :: TaffyIO ASUSInfo
getASUSInfoState = do
  ASUSInfoChanVar (_, theVar) <- getASUSInfoChanVar
  lift $ readMVar theVar

-- | Get a broadcast channel for ASUS info updates.
getASUSInfoChan :: TaffyIO (TChan ASUSInfo)
getASUSInfoChan = do
  ASUSInfoChanVar (chan, _) <- getASUSInfoChanVar
  return chan

getASUSInfoChanVar :: TaffyIO ASUSInfoChanVar
getASUSInfoChanVar =
  getStateDefault $ ASUSInfoChanVar <$> monitorASUSInfo

monitorASUSInfo :: TaffyIO (TChan ASUSInfo, MVar ASUSInfo)
monitorASUSInfo = do
  infoVar <- lift $ newMVar unknownASUSInfo
  chan <- liftIO newBroadcastTChanIO
  taffyFork $ do
    ctx <- ask
    let updateInfo = updateASUSInfo chan infoVar
        signalCallback _ _ _ _ = runReaderT updateInfo ctx
    _ <- registerForASUSPropertiesChanged signalCallback
    -- Do an initial update
    updateInfo
    -- Then poll every 2 seconds for CPU freq/temp changes
    lift $ forever $ do
      threadDelay 2_000_000
      runReaderT updateInfo ctx
  return (chan, infoVar)

registerForASUSPropertiesChanged
  :: (Signal -> String -> Map String Variant -> [String] -> IO ())
  -> ReaderT Context IO SignalHandler
registerForASUSPropertiesChanged signalHandler = do
  client <- asks systemDBusClient
  lift $ DBus.registerForPropertiesChanged
    client
    matchAny { matchInterface = Just asusInterfaceName
             , matchPath = Just asusObjectPath
             }
    signalHandler

updateASUSInfo
  :: TChan ASUSInfo
  -> MVar ASUSInfo
  -> TaffyIO ()
updateASUSInfo chan var = do
  info <- getASUSInfo
  lift $ do
    void $ swapMVar var info
    atomically $ writeTChan chan info
