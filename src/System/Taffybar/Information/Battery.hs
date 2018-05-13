{-# LANGUAGE OverloadedStrings #-}
-- | This is a simple library to query the Linux UPower daemon (via DBus) for
-- battery information.
module System.Taffybar.Information.Battery (
  -- * Types
    BatteryInfo(..)
  , BatteryState(..)
  , BatteryTechnology(..)
  , BatteryType(..)
  , module System.Taffybar.Information.Battery
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           DBus
import           DBus.Client
import           DBus.Internal.Types (Serial(..))
import qualified DBus.TH as DBus
import           Data.Either.Combinators
import           Data.Int
import           Data.List
import           Data.Map ( Map )
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.Word
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.DBus.Client.Params
import           System.Taffybar.DBus.Client.UPower
import           System.Taffybar.DBus.Client.UPowerDevice
import           System.Taffybar.Util
import           Text.Printf

batteryLog = logM "System.Taffybar.Information.Battery"

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
           (methodCall battPath "org.freedesktop.UPower.Device" "Fakte")
           { methodCallDestination = Just "org.freedesktop.UPower" }
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
  DisplayBatteryChanVar (Chan BatteryInfo, MVar BatteryInfo)

getDisplayBatteryInfo :: TaffyIO BatteryInfo
getDisplayBatteryInfo = do
  DisplayBatteryChanVar (_, theVar) <- getDisplayBatteryChanVar
  lift $ readMVar theVar

getDisplayBatteryChanVar :: TaffyIO DisplayBatteryChanVar
getDisplayBatteryChanVar =
  getStateDefault $ DisplayBatteryChanVar <$> monitorDisplayBattery

getDisplayBatteryChan :: TaffyIO (Chan BatteryInfo)
getDisplayBatteryChan = do
  DisplayBatteryChanVar (chan, _) <- getDisplayBatteryChanVar
  return chan

updateBatteryInfo
  :: Chan BatteryInfo
  -> MVar BatteryInfo
  -> ObjectPath
  -> TaffyIO ()
updateBatteryInfo chan var path =
  getBatteryInfo path >>= lift . either warnOfFailure doWrites
  where
    doWrites info =
        batteryLog DEBUG (printf "Writing info %s" $ show info) >>
        swapMVar var info >> writeChan chan info
    warnOfFailure e =
      batteryLog WARNING $ "Failed to update battery info " ++ show e

monitorDisplayBattery :: TaffyIO (Chan BatteryInfo, MVar BatteryInfo)
monitorDisplayBattery = do
  lift $ batteryLog DEBUG "Starting Battery Monitor"
  client <- asks systemDBusClient
  infoVar <- lift $ newMVar $ infoMapToBatteryInfo M.empty
  chan <- lift newChan
  taffyFork $ do
    lift $ batteryLog DEBUG "Started battery monitor thread"
    ctx <- ask
    let warnOfFailedGetDevice err =
          batteryLog WARNING "Failed to get composite battery device" >>
          return "/org/freedesktop/UPower/devices/DisplayDevice"
    displayPath <- lift $ getDisplayDevice client >>= either warnOfFailedGetDevice return
    let doUpdate = updateBatteryInfo chan infoVar displayPath
        signalCallback _ _ _ _ = runReaderT doUpdate ctx
    let propMatcher =
          matchAny
          { matchPath = Just displayPath
          }
    _ <- lift $ DBus.registerForPropertiesChanged client propMatcher signalCallback
    doUpdate
    return ()
  return (chan, infoVar)
