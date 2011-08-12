{-# LANGUAGE OverloadedStrings #-}
-- | This is a simple library to query the Linux UPower daemon (via
-- DBus) for battery information.  Currently, it only retrieves
-- information for the first battery it finds.
module System.Information.Battery (
  -- * Types
  BatteryContext,
  BatteryInfo(..),
  BatteryState(..),
  BatteryTechnology(..),
  BatteryType(..),
  -- * Accessors
  batteryContextNew,
  getBatteryInfo
  ) where

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Word
import Data.Int
import DBus.Client.Simple
import Data.List ( find )
import Data.Text ( isInfixOf, Text )

-- | An opaque wrapper around some internal library state
newtype BatteryContext = BC Proxy

data BatteryType = BatteryTypeUnknown
                 | BatteryTypeLinePower
                 | BatteryTypeBatteryType
                 | BatteryTypeUps
                 | BatteryTypeMonitor
                 | BatteryTypeMouse
                 | BatteryTypeKeyboard
                 | BatteryTypePda
                 | BatteryTypePhone
                 deriving (Show, Ord, Eq, Enum)

data BatteryState = BatteryStateUnknown
                  | BatteryStateCharging
                  | BatteryStateDischarging
                  | BatteryStateEmpty
                  | BatteryStateFullyCharged
                  | BatteryStatePendingCharge
                  | BatteryStatePendingDischarge
                  deriving (Show, Ord, Eq, Enum)

data BatteryTechnology = BatteryTechnologyUnknown
                       | BatteryTechnologyLithiumIon
                       | BatteryTechnologyLithiumPolymer
                       | BatteryTechnologyLithiumIronPhosphate
                       | BatteryTechnologyLeadAcid
                       | BatteryTechnologyNickelCadmium
                       | BatteryTechnologyNickelMetalHydride
                       deriving (Show, Ord, Eq, Enum)

-- | There are a few fields supported by UPower that aren't exposed
-- here.. could be easily.
data BatteryInfo = BatteryInfo { batteryNativePath :: Text
                               , batteryVendor :: Text
                               , batteryModel :: Text
                               , batterySerial :: Text
                               -- , batteryUpdateTime :: Time
                               , batteryType :: BatteryType
                               , batteryPowerSupply :: Bool
                               , batteryHasHistory :: Bool
                               , batteryHasStatistics :: Bool
                               , batteryOnline :: Bool
                               , batteryEnergy :: Double
                               , batteryEnergyEmpty :: Double
                               , batteryEnergyFull :: Double
                               , batteryEnergyFullDesign :: Double
                               , batteryEnergyRate :: Double
                               , batteryVoltage :: Double
                               , batteryTimeToEmpty :: Int64
                               , batteryTimeToFull :: Int64
                               , batteryPercentage :: Double
                               , batteryIsPresent :: Bool
                               , batteryState :: BatteryState
                               , batteryIsRechargable :: Bool
                               , batteryCapacity :: Double
                               , batteryTechnology :: BatteryTechnology
{-                               , batteryRecallNotice :: Bool
                               , batteryRecallVendor :: Text
                               , batteryRecallUr :: Text
-}
                               }

-- | Find the first power source that is a battery in the list.  The
-- simple heuristic is a substring search on 'BAT'
firstBattery :: [ObjectPath] -> Maybe ObjectPath
firstBattery = find (isInfixOf "BAT" . objectPathText)

-- | The name of the power daemon bus
powerBusName :: BusName
powerBusName = "org.freedesktop.UPower"

-- | The base object path
powerBaseObjectPath :: ObjectPath
powerBaseObjectPath = "/org/freedesktop/UPower"

-- | A helper to read the variant contents of a dict with a default
-- value.
readDict :: (IsVariant a) => Map Text Variant -> Text -> a -> a
readDict dict key dflt = val
  where
    Just val = fromVariant variant
    variant = M.findWithDefault (toVariant dflt) key dict

-- | Query the UPower daemon about information on a specific battery.
-- If some fields are not actually present, they may have bogus values
-- here.  Don't bet anything critical on it.
getBatteryInfo :: BatteryContext -> IO BatteryInfo
getBatteryInfo (BC batteryProxy) = do
  -- Grab all of the properties of the battery each call with one
  -- message.
  let iface :: Variant
      iface = toVariant ("org.freedesktop.UPower.Device" :: Text)

  [val] <- call batteryProxy "org.freedesktop.DBus.Properties" "GetAll" [iface]

  let dict :: Map Text Variant
      Just dict = fromVariant val
  return BatteryInfo { batteryNativePath = readDict dict "NativePath" ""
                     , batteryVendor = readDict dict "Vendor" ""
                     , batteryModel = readDict dict "Model" ""
                     , batterySerial = readDict dict "Serial" ""
                     , batteryType = toEnum $ fromIntegral $ readDict dict "Type" (0 :: Word64)
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
                     , batteryState = toEnum $ fromIntegral $ readDict dict "State" (0 :: Word64)
                     , batteryIsRechargable = readDict dict "IsRechargable" True
                     , batteryCapacity = readDict dict "Capacity" 0.0
                     , batteryTechnology =
                       toEnum $ fromIntegral $ readDict dict "Technology" (0 :: Word64)
                     }

-- | Construct a battery context if possible.  This could fail if the
-- UPower daemon is not running.  The context can be used to get
-- actual battery state with 'getBatteryInfo'.
batteryContextNew :: IO (Maybe BatteryContext)
batteryContextNew = do
  systemConn <- connectSystem

  -- First, get the list of devices.  For now, we just get the stats
  -- for the first battery
  powerProxy <- proxy systemConn powerBusName powerBaseObjectPath
  [ powerDevicesV ] <- call powerProxy "org.freedesktop.UPower" "EnumerateDevices" []
  let Just powerDevices = fromVariant powerDevicesV
  case firstBattery powerDevices of
    Nothing -> return Nothing
    Just battPath ->
      proxy systemConn powerBusName battPath >>= (return . Just . BC)

