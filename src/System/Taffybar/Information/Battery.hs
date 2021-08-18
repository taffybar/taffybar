{-# LANGUAGE OverloadedStrings #-}
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
-- UPower dbus, as well as a "BroadcastChan" system for allowing multiple
-- readers to receive 'BatteryState' updates without duplicating requests.
-----------------------------------------------------------------------------
module System.Taffybar.Information.Battery
  ( BatteryInfo(..)
  , BatteryState(..)
  , BatteryTechnology(..)
  , BatteryType(..)
  , module System.Taffybar.Information.Battery
  ) where

import           BroadcastChan
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           DBus
import           DBus.Client
import           DBus.Internal.Types (Serial(..))
import qualified DBus.TH as DBus
import           Data.Int
import           Data.List
import           Data.Map ( Map )
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text ( Text )
import           Data.Word
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.DBus.Client.Params
import           System.Taffybar.DBus.Client.UPower
import           System.Taffybar.DBus.Client.UPowerDevice
import           System.Taffybar.Util

batteryLogPath :: String
batteryLogPath = "System.Taffybar.Information.Battery"

batteryLog
  :: MonadIO m
  => Priority -> String -> m ()
batteryLog priority = liftIO . logM batteryLogPath priority

batteryLogF
  :: (MonadIO m, Show t)
  => Priority -> String -> t -> m ()
batteryLogF = logPrintF batteryLogPath

-- | The prefix of name of battery devices path. UPower generates the object
-- path as "battery" + "_" + basename of the sysfs object.
batteryPrefix :: String
batteryPrefix = formatObjectPath uPowerBaseObjectPath ++ "/devices/battery_"

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
    TypeWord8   -> return $ fromIntegral (f v :: Word8)
    TypeWord16  -> return $ fromIntegral (f v :: Word16)
    TypeWord32  -> return $ fromIntegral (f v :: Word32)
    TypeWord64  -> return $ fromIntegral (f v :: Word64)
    TypeInt16   -> return $ fromIntegral (f v :: Int16)
    TypeInt32   -> return $ fromIntegral (f v :: Int32)
    TypeInt64   -> return $ fromIntegral (f v :: Int64)
    _           -> Nothing
  where
    f :: (Num a, IsVariant a) => Variant -> a
    f = fromMaybe (fromIntegral dflt) . fromVariant

-- XXX: Remove this once it is exposed in haskell-dbus
dummyMethodError :: MethodError
dummyMethodError = methodError (Serial 1) $ errorName_ "org.ClientTypeMismatch"

-- | Query the UPower daemon about information on a specific battery.
-- If some fields are not actually present, they may have bogus values
-- here.  Don't bet anything critical on it.
getBatteryInfo :: ObjectPath -> TaffyIO (Either MethodError BatteryInfo)
getBatteryInfo battPath = asks systemDBusClient >>= \client -> lift $ runExceptT $ do
  reply <- ExceptT $ getAllProperties client $
           (methodCall battPath uPowerDeviceInterfaceName "FakeMethod")
           { methodCallDestination = Just uPowerBusName }
  dict <- ExceptT $ return $ maybeToEither dummyMethodError $
         listToMaybe (methodReturnBody reply) >>= fromVariant
  return $ infoMapToBatteryInfo dict

infoMapToBatteryInfo :: Map Text Variant -> BatteryInfo
infoMapToBatteryInfo dict =
    BatteryInfo
      { batteryNativePath = readDict dict "NativePath" ""
      , batteryVendor = readDict dict "Vendor" ""
      , batteryModel = readDict dict "Model" ""
      , batterySerial = readDict dict "Serial" ""
      , batteryType = toEnum $ fromIntegral $ readDictIntegral dict "Type" 0
      , batteryPowerSupply = readDict dict "PowerSupply" False
      , batteryHasHistory = readDict dict "HasHistory" False
      , batteryHasStatistics = readDict dict "HasStatistics" False
      , batteryOnline = readDict dict "Online" False
      , batteryEnergy = readDict dict "Energy" 0.0
      , batteryEnergyEmpty = readDict dict "EnergyEmpty" 0.0
      , batteryEnergyFull = readDict dict "EnergyFull" 0.0
      , batteryEnergyFullDesign = readDict dict "EnergyFullDesign" 0.0
      , batteryEnergyRate = readDict dict "EnergyRate" 0.0
      , batteryVoltage = readDict dict "Voltage" 0.0
      , batteryTimeToEmpty = readDict dict "TimeToEmpty" 0
      , batteryTimeToFull = readDict dict "TimeToFull" 0
      , batteryPercentage = readDict dict "Percentage" 0.0
      , batteryIsPresent = readDict dict "IsPresent" False
      , batteryState = toEnum $ readDictIntegral dict "State" 0
      , batteryIsRechargeable = readDict dict "IsRechargable" True
      , batteryCapacity = readDict dict "Capacity" 0.0
      , batteryTechnology =
          toEnum $ fromIntegral $ readDictIntegral dict "Technology" 0
      , batteryUpdateTime = readDict dict "UpdateTime" 0
      , batteryLuminosity = readDict dict "Luminosity" 0.0
      , batteryTemperature = readDict dict "Temperature" 0.0
      , batteryWarningLevel = readDict dict "WarningLevel" 0
      , batteryBatteryLevel = readDict dict "BatteryLevel" 0
      , batteryIconName = readDict dict "IconName" ""
      }

getBatteryPaths :: TaffyIO (Either MethodError [ObjectPath])
getBatteryPaths = do
  client <- asks systemDBusClient
  liftIO $ runExceptT $ do
    paths <- ExceptT $ enumerateDevices client
    return $ filter isBattery paths

newtype DisplayBatteryChanVar =
  DisplayBatteryChanVar (BroadcastChan In BatteryInfo, MVar BatteryInfo)

getDisplayBatteryInfo :: TaffyIO BatteryInfo
getDisplayBatteryInfo = do
  DisplayBatteryChanVar (_, theVar) <- getDisplayBatteryChanVar
  lift $ readMVar theVar

defaultMonitorDisplayBatteryProperties :: [String]
defaultMonitorDisplayBatteryProperties = [ "IconName", "State", "Percentage" ]

-- | Start the monitoring of the display battery, and setup the associated
-- channel and mvar for the current state.
setupDisplayBatteryChanVar :: [String] -> TaffyIO DisplayBatteryChanVar
setupDisplayBatteryChanVar properties = getStateDefault $
  DisplayBatteryChanVar <$> monitorDisplayBattery properties

getDisplayBatteryChanVar :: TaffyIO DisplayBatteryChanVar
getDisplayBatteryChanVar =
  setupDisplayBatteryChanVar defaultMonitorDisplayBatteryProperties

getDisplayBatteryChan :: TaffyIO (BroadcastChan In BatteryInfo)
getDisplayBatteryChan = do
  DisplayBatteryChanVar (chan, _) <- getDisplayBatteryChanVar
  return chan

updateBatteryInfo
  :: BroadcastChan In BatteryInfo
  -> MVar BatteryInfo
  -> ObjectPath
  -> TaffyIO ()
updateBatteryInfo chan var path =
  getBatteryInfo path >>= lift . either warnOfFailure doWrites
  where
    doWrites info =
        batteryLogF DEBUG "Writing info %s" info >>
        swapMVar var info >> void (writeBChan chan info)
    warnOfFailure = batteryLogF WARNING "Failed to update battery info %s"

registerForAnyUPowerPropertiesChanged
  :: (Signal -> String -> Map String Variant -> [String] -> IO ())
  -> ReaderT Context IO SignalHandler
registerForAnyUPowerPropertiesChanged = registerForUPowerPropertyChanges []

registerForUPowerPropertyChanges
  :: [String]
  -> (Signal -> String -> Map String Variant -> [String] -> IO ())
  -> ReaderT Context IO SignalHandler
registerForUPowerPropertyChanges properties signalHandler = do
  client <- asks systemDBusClient
  lift $ DBus.registerForPropertiesChanged
      client
      matchAny { matchInterface = Just uPowerDeviceInterfaceName }
      handleIfPropertyMatches
  where handleIfPropertyMatches rawSignal n propertiesMap l =
          let propertyPresent prop = isJust $ M.lookup prop propertiesMap
          in when (any propertyPresent properties || null properties) $
             signalHandler rawSignal n propertiesMap l

-- | Monitor the DisplayDevice for changes, writing a new "BatteryInfo" object
-- to returned "MVar" and "Chan" objects
monitorDisplayBattery ::
  [String] -> TaffyIO (BroadcastChan In BatteryInfo, MVar BatteryInfo)
monitorDisplayBattery propertiesToMonitor = do
  lift $ batteryLog DEBUG "Starting Battery Monitor"
  client <- asks systemDBusClient
  infoVar <- lift $ newMVar $ infoMapToBatteryInfo M.empty
  chan <- newBroadcastChan
  taffyFork $ do
    ctx <- ask
    let warnOfFailedGetDevice err =
          batteryLogF WARNING "Failure getting DisplayBattery: %s" err >>
          return "/org/freedesktop/UPower/devices/DisplayDevice"
    displayPath <- lift $ getDisplayDevice client >>=
                   either warnOfFailedGetDevice return
    let doUpdate = updateBatteryInfo chan infoVar displayPath
        signalCallback _ _ changedProps _ =
          do
            batteryLogF DEBUG "Battery changed properties: %s" changedProps
            runReaderT doUpdate ctx
    _ <- registerForUPowerPropertyChanges propertiesToMonitor signalCallback
    doUpdate
    return ()
  return (chan, infoVar)

-- | Call "refreshAllBatteries" whenever the BatteryInfo for the DisplayDevice
-- is updated. This handles cases where there is a race between the signal that
-- something is updated and the update actually being visible. See
-- https://github.com/taffybar/taffybar/issues/330 for more details.
refreshBatteriesOnPropChange :: TaffyIO ()
refreshBatteriesOnPropChange = ask >>= \ctx ->
  let updateIfRealChange _ _ changedProps _ =
        flip runReaderT ctx $
             when (any ((`notElem` ["UpdateTime", "Voltage"]) . fst) $
                       M.toList changedProps) $
                  lift (threadDelay 1000000) >> refreshAllBatteries
  in void $ registerForAnyUPowerPropertiesChanged updateIfRealChange

-- | Request a refresh of all UPower batteries. This is only needed if UPower's
-- refresh mechanism is not working properly.
refreshAllBatteries :: TaffyIO ()
refreshAllBatteries = do
  client <- asks systemDBusClient
  let doRefresh path =
        batteryLogF DEBUG "Refreshing battery: %s" path >> refresh client path
  eerror <- runExceptT $ ExceptT getBatteryPaths >>= liftIO . mapM doRefresh
  let logRefreshError = batteryLogF ERROR "Failed to refresh battery: %s"
      logGetPathsError = batteryLogF ERROR "Failed to get battery paths %s"

  void $ either logGetPathsError (mapM_ $ either logRefreshError return) eerror
