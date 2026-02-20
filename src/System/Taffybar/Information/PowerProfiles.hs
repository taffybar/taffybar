{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.PowerProfiles
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides information about the current power profile using
-- the power-profiles-daemon DBus API (net.hadess.PowerProfiles).
module System.Taffybar.Information.PowerProfiles
  ( PowerProfile (..),
    PowerProfileInfo (..),
    getPowerProfileInfo,
    getPowerProfileInfoFromClient,
    getPowerProfileInfoChan,
    getPowerProfileInfoState,
    cycleProfile,
    setProfile,
    powerProfileToString,
    stringToPowerProfile,
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
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import System.Log.Logger
import System.Taffybar.Context
import System.Taffybar.Util (logPrintF, maybeToEither)

-- | Power profile modes supported by power-profiles-daemon.
data PowerProfile
  = PowerSaver
  | Balanced
  | Performance
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Information about the current power profile state.
data PowerProfileInfo = PowerProfileInfo
  { currentProfile :: PowerProfile,
    availableProfiles :: [PowerProfile],
    performanceDegraded :: Maybe Text
  }
  deriving (Eq, Show)

-- | The DBus bus name for power-profiles-daemon.
powerProfilesBusName :: BusName
powerProfilesBusName = "net.hadess.PowerProfiles"

-- | The DBus object path for power-profiles-daemon.
powerProfilesObjectPath :: ObjectPath
powerProfilesObjectPath = "/net/hadess/PowerProfiles"

-- | The DBus interface name for power-profiles-daemon.
powerProfilesInterfaceName :: InterfaceName
powerProfilesInterfaceName = "net.hadess.PowerProfiles"

powerProfilesLogPath :: String
powerProfilesLogPath = "System.Taffybar.Information.PowerProfiles"

powerProfilesLogF :: (MonadIO m, Show t) => Priority -> String -> t -> m ()
powerProfilesLogF = logPrintF powerProfilesLogPath

-- | Convert a PowerProfile to its DBus string representation.
powerProfileToString :: PowerProfile -> Text
powerProfileToString PowerSaver = "power-saver"
powerProfileToString Balanced = "balanced"
powerProfileToString Performance = "performance"

-- | Parse a DBus string to a PowerProfile.
stringToPowerProfile :: Text -> Maybe PowerProfile
stringToPowerProfile "power-saver" = Just PowerSaver
stringToPowerProfile "balanced" = Just Balanced
stringToPowerProfile "performance" = Just Performance
stringToPowerProfile _ = Nothing

-- | Default info when power-profiles-daemon is unavailable.
unknownProfileInfo :: PowerProfileInfo
unknownProfileInfo =
  PowerProfileInfo
    { currentProfile = Balanced,
      availableProfiles = [],
      performanceDegraded = Nothing
    }

-- XXX: Remove this once it is exposed in haskell-dbus
dummyMethodError :: MethodError
dummyMethodError = methodError (Serial 1) $ errorName_ "org.ClientTypeMismatch"

readDictMaybe :: (IsVariant a) => Map Text Variant -> Text -> Maybe a
readDictMaybe dict key = M.lookup key dict >>= fromVariant

getProperties ::
  Client ->
  IO (Either MethodError (Map Text Variant))
getProperties client = runExceptT $ do
  reply <-
    ExceptT $
      getAllProperties client $
        (methodCall powerProfilesObjectPath powerProfilesInterfaceName "FakeMethod")
          { methodCallDestination = Just powerProfilesBusName
          }
  ExceptT $
    return $
      maybeToEither dummyMethodError $
        listToMaybe (methodReturnBody reply) >>= fromVariant

-- | Get current power profile info using the system DBus client from Context.
getPowerProfileInfo :: TaffyIO PowerProfileInfo
getPowerProfileInfo = getSystemDBusClient >>= liftIO . getPowerProfileInfoFromClient

-- | Get current power profile info from a DBus client.
getPowerProfileInfoFromClient :: Client -> IO PowerProfileInfo
getPowerProfileInfoFromClient client = do
  propsResult <- getProperties client
  case propsResult of
    Left err -> do
      powerProfilesLogF WARNING "Failed to read power profiles properties: %s" err
      return unknownProfileInfo
    Right props -> do
      let activeProfileStr = readDictMaybe props "ActiveProfile" :: Maybe Text
          profilesArray = readDictMaybe props "Profiles" :: Maybe [Map Text Variant]
          degraded = readDictMaybe props "PerformanceDegraded" :: Maybe Text

          -- Parse available profiles from the array of dicts
          parseProfile :: Map Text Variant -> Maybe PowerProfile
          parseProfile m = do
            name <- readDictMaybe m "Profile"
            stringToPowerProfile name

          availableProfs = maybe [] (mapMaybe parseProfile) profilesArray
          currentProf = fromMaybe Balanced (activeProfileStr >>= stringToPowerProfile)

      return
        PowerProfileInfo
          { currentProfile = currentProf,
            availableProfiles = availableProfs,
            performanceDegraded = if degraded == Just "" then Nothing else degraded
          }

-- | Set the active power profile.
setProfile :: Client -> PowerProfile -> IO (Either MethodError ())
setProfile client profile = do
  let profileStr = powerProfileToString profile
  result <-
    setProperty
      client
      (methodCall powerProfilesObjectPath powerProfilesInterfaceName "ActiveProfile")
        { methodCallDestination = Just powerProfilesBusName
        }
      (toVariant profileStr)
  return $ case result of
    Left err -> Left err
    Right _ -> Right ()

-- | Cycle to the next power profile.
-- Order: power-saver -> balanced -> performance -> power-saver
cycleProfile :: Client -> PowerProfileInfo -> IO (Either MethodError ())
cycleProfile client info =
  let current = currentProfile info
      -- Find next profile in cycle
      nextProfile = case current of
        PowerSaver -> Balanced
        Balanced -> Performance
        Performance -> PowerSaver
   in setProfile client nextProfile

-- State management for monitoring

newtype PowerProfileInfoChanVar
  = PowerProfileInfoChanVar (TChan PowerProfileInfo, MVar PowerProfileInfo)

-- | Get the current power profile info state.
getPowerProfileInfoState :: TaffyIO PowerProfileInfo
getPowerProfileInfoState = do
  PowerProfileInfoChanVar (_, theVar) <- getPowerProfileInfoChanVar
  lift $ readMVar theVar

-- | Get a broadcast channel for power profile info updates.
getPowerProfileInfoChan :: TaffyIO (TChan PowerProfileInfo)
getPowerProfileInfoChan = do
  PowerProfileInfoChanVar (chan, _) <- getPowerProfileInfoChanVar
  return chan

getPowerProfileInfoChanVar :: TaffyIO PowerProfileInfoChanVar
getPowerProfileInfoChanVar =
  getStateDefault $ PowerProfileInfoChanVar <$> monitorPowerProfileInfo

monitorPowerProfileInfo :: TaffyIO (TChan PowerProfileInfo, MVar PowerProfileInfo)
monitorPowerProfileInfo = do
  infoVar <- lift $ newMVar unknownProfileInfo
  chan <- liftIO newBroadcastTChanIO
  taffyFork $ do
    ctx <- ask
    let updateInfo = updatePowerProfileInfo chan infoVar
        signalCallback _ _ _ _ = runReaderT updateInfo ctx
    _ <- registerForPowerProfilesPropertiesChanged signalCallback
    updateInfo
  return (chan, infoVar)

registerForPowerProfilesPropertiesChanged ::
  (Signal -> String -> Map String Variant -> [String] -> IO ()) ->
  ReaderT Context IO SignalHandler
registerForPowerProfilesPropertiesChanged signalHandler = do
  client <- getSystemDBusClient
  lift $
    DBus.registerForPropertiesChanged
      client
      matchAny
        { matchInterface = Just powerProfilesInterfaceName,
          matchPath = Just powerProfilesObjectPath
        }
      signalHandler

updatePowerProfileInfo ::
  TChan PowerProfileInfo ->
  MVar PowerProfileInfo ->
  TaffyIO ()
updatePowerProfileInfo chan var = do
  info <- getPowerProfileInfo
  lift $ do
    _ <- swapMVar var info
    atomically $ writeTChan chan info
