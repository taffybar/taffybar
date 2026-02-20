{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.Battery
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides functions for querying battery information using the
-- UPower dbus, as well as a broadcast "TChan" system for allowing multiple
-- readers to receive 'BatteryState' updates without duplicating requests.
module System.Taffybar.Information.Battery
  ( BatteryInfo (..),
    BatteryState (..),
    BatteryTechnology (..),
    BatteryType (..),
    module System.Taffybar.Information.Battery,
  )
where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import DBus
import DBus.Client
import DBus.Internal.Types (Serial (..))
import qualified DBus.TH as DBus
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Word
import System.Log.Logger
import System.Taffybar.Context
import System.Taffybar.DBus.Client.Params
import System.Taffybar.DBus.Client.UPower
import System.Taffybar.DBus.Client.UPowerDevice
import System.Taffybar.Util

-- | Logger namespace for battery modules.
batteryLogPath :: String
batteryLogPath = "System.Taffybar.Information.Battery"

-- | Log a message to the battery logger.
batteryLog ::
  (MonadIO m) =>
  Priority -> String -> m ()
batteryLog priority = liftIO . logM batteryLogPath priority

-- | Formatted logging helper for the battery logger.
batteryLogF ::
  (MonadIO m, Show t) =>
  Priority -> String -> t -> m ()
batteryLogF = logPrintF batteryLogPath

-- | The DBus path prefix where UPower enumerates its objects.
uPowerDevicesPath :: ObjectPath
uPowerDevicesPath = objectPath_ (formatObjectPath uPowerBaseObjectPath ++ "/devices")

-- | The prefix of name of battery devices path. UPower generates the object
-- path as "battery" + "_" + basename of the sysfs object.
batteryPrefix :: String
batteryPrefix = formatObjectPath uPowerDevicesPath ++ "/battery_"

-- | Determine if a power source is a battery.
isBattery :: ObjectPath -> Bool
isBattery = isPrefixOf batteryPrefix . formatObjectPath

-- | A helper to read the variant contents of a dict with a default
-- value.
readDict :: (IsVariant a) => Map Text Variant -> Text -> a -> a
readDict dict key dflt = fromMaybe dflt $ do
  variant <- M.lookup key dict
  fromVariant variant

-- | Read the variant contents of a dict which is of an unknown integral type.
readDictIntegral :: Map Text Variant -> Text -> Int32 -> Int
readDictIntegral dict key dflt = fromMaybe (fromIntegral dflt) $ do
  v <- M.lookup key dict
  case variantType v of
    TypeWord8 -> return $ fromIntegral (f v :: Word8)
    TypeWord16 -> return $ fromIntegral (f v :: Word16)
    TypeWord32 -> return $ fromIntegral (f v :: Word32)
    TypeWord64 -> return $ fromIntegral (f v :: Word64)
    TypeInt16 -> return $ fromIntegral (f v :: Int16)
    TypeInt32 -> return $ fromIntegral (f v :: Int32)
    TypeInt64 -> return $ fromIntegral (f v :: Int64)
    _ -> Nothing
  where
    f :: (Num a, IsVariant a) => Variant -> a
    f = fromMaybe (fromIntegral dflt) . fromVariant

-- XXX: Remove this once it is exposed in haskell-dbus

-- | Placeholder 'MethodError' used when required reply payload is missing.
dummyMethodError :: MethodError
dummyMethodError = methodError (Serial 1) $ errorName_ "org.ClientTypeMismatch"

-- | Query the UPower daemon about information on a specific battery.
-- If some fields are not actually present, they may have bogus values
-- here.  Don't bet anything critical on it.
getBatteryInfo :: ObjectPath -> TaffyIO (Either MethodError BatteryInfo)
getBatteryInfo battPath =
  asks systemDBusClient >>= \client -> lift $ runExceptT $ do
    reply <-
      ExceptT $
        getAllProperties client $
          (methodCall battPath uPowerDeviceInterfaceName "FakeMethod")
            { methodCallDestination = Just uPowerBusName
            }
    dict <-
      ExceptT $
        return $
          maybeToEither dummyMethodError $
            listToMaybe (methodReturnBody reply) >>= fromVariant
    return $ infoMapToBatteryInfo dict

-- | Decode a UPower property map into a 'BatteryInfo' record.
infoMapToBatteryInfo :: Map Text Variant -> BatteryInfo
infoMapToBatteryInfo dict =
  BatteryInfo
    { batteryNativePath = readDict dict "NativePath" "",
      batteryVendor = readDict dict "Vendor" "",
      batteryModel = readDict dict "Model" "",
      batterySerial = readDict dict "Serial" "",
      batteryType = toEnum $ fromIntegral $ readDictIntegral dict "Type" 0,
      batteryPowerSupply = readDict dict "PowerSupply" False,
      batteryHasHistory = readDict dict "HasHistory" False,
      batteryHasStatistics = readDict dict "HasStatistics" False,
      batteryOnline = readDict dict "Online" False,
      batteryEnergy = readDict dict "Energy" 0.0,
      batteryEnergyEmpty = readDict dict "EnergyEmpty" 0.0,
      batteryEnergyFull = readDict dict "EnergyFull" 0.0,
      batteryEnergyFullDesign = readDict dict "EnergyFullDesign" 0.0,
      batteryEnergyRate = readDict dict "EnergyRate" 0.0,
      batteryVoltage = readDict dict "Voltage" 0.0,
      batteryTimeToEmpty = readDict dict "TimeToEmpty" 0,
      batteryTimeToFull = readDict dict "TimeToFull" 0,
      batteryPercentage = readDict dict "Percentage" 0.0,
      batteryIsPresent = readDict dict "IsPresent" False,
      batteryState = toEnum $ readDictIntegral dict "State" 0,
      batteryIsRechargeable = readDict dict "IsRechargable" True,
      batteryCapacity = readDict dict "Capacity" 0.0,
      batteryTechnology =
        toEnum $ fromIntegral $ readDictIntegral dict "Technology" 0,
      batteryUpdateTime = readDict dict "UpdateTime" 0,
      batteryLuminosity = readDict dict "Luminosity" 0.0,
      batteryTemperature = readDict dict "Temperature" 0.0,
      batteryWarningLevel = readDict dict "WarningLevel" 0,
      batteryBatteryLevel = readDict dict "BatteryLevel" 0,
      batteryIconName = readDict dict "IconName" ""
    }

-- | Enumerate UPower object paths that represent batteries.
getBatteryPaths :: TaffyIO (Either MethodError [ObjectPath])
getBatteryPaths = do
  client <- asks systemDBusClient
  liftIO $ runExceptT $ do
    paths <- ExceptT $ enumerateDevices client
    return $ filter isBattery paths

-- | Shared display-battery update channel and latest-value cache.
newtype DisplayBatteryChanVar
  = DisplayBatteryChanVar (TChan BatteryInfo, MVar BatteryInfo)

-- | Read the latest display battery snapshot.
getDisplayBatteryInfo :: TaffyIO BatteryInfo
getDisplayBatteryInfo = do
  DisplayBatteryChanVar (_, theVar) <- getDisplayBatteryChanVar
  lift $ readMVar theVar

-- | Default set of UPower properties that trigger display-battery refreshes.
defaultMonitorDisplayBatteryProperties :: [String]
defaultMonitorDisplayBatteryProperties = ["IconName", "State", "Percentage"]

-- | Start the monitoring of the display battery, and setup the associated
-- channel and mvar for the current state.
setupDisplayBatteryChanVar :: [String] -> TaffyIO DisplayBatteryChanVar
setupDisplayBatteryChanVar properties =
  getStateDefault $
    DisplayBatteryChanVar <$> monitorDisplayBattery properties

-- | Get (or initialize) the shared display-battery channel+state holder.
getDisplayBatteryChanVar :: TaffyIO DisplayBatteryChanVar
getDisplayBatteryChanVar =
  setupDisplayBatteryChanVar defaultMonitorDisplayBatteryProperties

-- | Get the broadcast channel for display-battery updates.
getDisplayBatteryChan :: TaffyIO (TChan BatteryInfo)
getDisplayBatteryChan = do
  DisplayBatteryChanVar (chan, _) <- getDisplayBatteryChanVar
  return chan

-- | Refresh battery info for a path and publish it to the shared state.
updateBatteryInfo ::
  TChan BatteryInfo ->
  MVar BatteryInfo ->
  ObjectPath ->
  TaffyIO ()
updateBatteryInfo chan var path =
  getBatteryInfo path >>= lift . either warnOfFailure doWrites
  where
    doWrites info =
      batteryLogF DEBUG "Writing info %s" info
        >> swapMVar var info
        >> void (atomically $ writeTChan chan info)
    warnOfFailure = batteryLogF WARNING "Failed to update battery info %s"

-- | Register for any UPower device property changes.
registerForAnyUPowerPropertiesChanged ::
  (Signal -> String -> Map String Variant -> [String] -> IO ()) ->
  ReaderT Context IO SignalHandler
registerForAnyUPowerPropertiesChanged = registerForUPowerPropertyChanges []

-- | Register for UPower device property changes, optionally filtering by
-- property names.
registerForUPowerPropertyChanges ::
  [String] ->
  (Signal -> String -> Map String Variant -> [String] -> IO ()) ->
  ReaderT Context IO SignalHandler
registerForUPowerPropertyChanges properties signalHandler = do
  client <- asks systemDBusClient
  lift $
    DBus.registerForPropertiesChanged
      client
      matchAny
        { matchInterface = Just uPowerDeviceInterfaceName,
          matchPathNamespace = Just uPowerDevicesPath
        }
      handleIfPropertyMatches
  where
    handleIfPropertyMatches rawSignal n propertiesMap l =
      let propertyPresent prop = M.member prop propertiesMap
       in when (any propertyPresent properties || null properties) $
            signalHandler rawSignal n propertiesMap l

-- | Monitor the DisplayDevice for changes, writing a new "BatteryInfo" object
-- to returned "MVar" and "Chan" objects
monitorDisplayBattery ::
  [String] -> TaffyIO (TChan BatteryInfo, MVar BatteryInfo)
monitorDisplayBattery propertiesToMonitor = do
  lift $ batteryLog DEBUG "Starting Battery Monitor"
  client <- asks systemDBusClient
  infoVar <- lift $ newMVar $ infoMapToBatteryInfo M.empty
  chan <- liftIO newBroadcastTChanIO
  taffyFork $ do
    ctx <- ask
    let warnOfFailedGetDevice err =
          batteryLogF WARNING "Failure getting DisplayBattery: %s" err
            >> return "/org/freedesktop/UPower/devices/DisplayDevice"
    displayPath <-
      lift $
        getDisplayDevice client
          >>= either warnOfFailedGetDevice return
    let doUpdate = updateBatteryInfo chan infoVar displayPath
        signalCallback _ _ changedProps _ =
          do
            batteryLogF DEBUG "Battery changed properties: %s" changedProps
            runReaderT doUpdate ctx
    _ <- registerForUPowerPropertyChanges propertiesToMonitor signalCallback
    doUpdate

  return (chan, infoVar)

-- | Call "refreshAllBatteries" whenever the BatteryInfo for the DisplayDevice
-- is updated. This handles cases where there is a race between the signal that
-- something is updated and the update actually being visible. See
-- https://github.com/taffybar/taffybar/issues/330 for more details.
refreshBatteriesOnPropChange :: TaffyIO ()
refreshBatteriesOnPropChange =
  ask >>= \ctx ->
    let updateIfRealChange _ _ changedProps _ =
          flip runReaderT ctx
            $ when
              ( any ((`notElem` ["UpdateTime", "Voltage"]) . fst) $
                  M.toList changedProps
              )
            $ lift (threadDelay 1000000) >> refreshAllBatteries
     in void $ registerForAnyUPowerPropertiesChanged updateIfRealChange

-- | Request a refresh of all UPower batteries. This is only needed if UPower's
-- refresh mechanism is not working properly.
refreshAllBatteries :: TaffyIO ()
refreshAllBatteries = do
  client <- asks systemDBusClient
  let doRefresh path =
        batteryLogF DEBUG "Refreshing battery: %s" path >> refresh client path
  eerror <- runExceptT $ ExceptT getBatteryPaths >>= liftIO . mapM doRefresh

  -- NB. The Refresh() method is only available if the UPower daemon
  -- was started in debug mode. So ignore any errors about the method
  -- not being implemented.
  let logRefreshError e =
        unless (methodErrorName e == errorUnknownMethod) $
          batteryLogF ERROR "Failed to refresh battery: %s" e
      logGetPathsError = batteryLogF ERROR "Failed to get battery paths %s"

  void $ either logGetPathsError (mapM_ $ either logRefreshError return) eerror
