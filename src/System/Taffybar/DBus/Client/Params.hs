{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.DBus.Client.Params where

import DBus
import DBus.Generation
import Language.Haskell.TH
import System.Taffybar.DBus.Client.Util

playerGenerationParams :: GenerationParams
playerGenerationParams = defaultGenerationParams
  { genTakeSignalErrorHandler = True
  , genObjectPath = Just "/org/mpris/MediaPlayer2"
  }


-- | The base object path for the UPower interface
uPowerBaseObjectPath :: ObjectPath
uPowerBaseObjectPath = "/org/freedesktop/UPower"

-- | The name of the power daemon bus
uPowerBusName :: BusName
uPowerBusName = "org.freedesktop.UPower"

uPowerGenerationParams :: GenerationParams
uPowerGenerationParams = defaultGenerationParams
  { genTakeSignalErrorHandler = True
  , genBusName = Just uPowerBusName
  }

data BatteryType
  = BatteryTypeUnknown
  | BatteryTypeLinePower
  | BatteryTypeBatteryType
  | BatteryTypeUps
  | BatteryTypeMonitor
  | BatteryTypeMouse
  | BatteryTypeKeyboard
  | BatteryTypePda
  | BatteryTypePhone
  deriving (Show, Ord, Eq, Enum)

data BatteryState
  = BatteryStateUnknown
  | BatteryStateDischarging
  | BatteryStateCharging
  | BatteryStateEmpty
  | BatteryStateFullyCharged
  | BatteryStatePendingCharge
  | BatteryStatePendingDischarge
  deriving (Show, Ord, Eq, Enum)

data BatteryTechnology
  = BatteryTechnologyUnknown
  | BatteryTechnologyLithiumIon
  | BatteryTechnologyLithiumPolymer
  | BatteryTechnologyLithiumIronPhosphate
  | BatteryTechnologyLeadAcid
  | BatteryTechnologyNickelCadmium
  | BatteryTechnologyNickelMetalHydride
  deriving (Show, Ord, Eq, Enum)

batteryTypeForName :: GetTypeForName
batteryTypeForName name = const $
  case name of
    "Type" -> yes ''BatteryType
    "State" -> yes ''BatteryState
    "Technology" -> yes ''BatteryTechnology
    _ -> Nothing
  where yes = Just . ConT
