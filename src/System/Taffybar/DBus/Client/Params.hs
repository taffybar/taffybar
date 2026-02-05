{-# LANGUAGE TemplateHaskellQuotes #-}
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

uPowerDeviceInterfaceName :: InterfaceName
uPowerDeviceInterfaceName = "org.freedesktop.UPower.Device"

uPowerGenerationParams :: GenerationParams
uPowerGenerationParams = defaultGenerationParams
  { genTakeSignalErrorHandler = True
  , genBusName = Just uPowerBusName
  }
-- | The bus name for PulseAudio's server lookup on the session bus.
paServerLookupBusName :: BusName
paServerLookupBusName = "org.PulseAudio1"

-- | The object path for PulseAudio's server lookup object.
paServerLookupObjectPath :: ObjectPath
paServerLookupObjectPath = "/org/pulseaudio/server_lookup1"

-- | The object path for PulseAudio's core object.
paCoreObjectPath :: ObjectPath
paCoreObjectPath = "/org/pulseaudio/core1"

paServerLookupInterfaceName :: InterfaceName
paServerLookupInterfaceName = "org.PulseAudio.ServerLookup1"

paCoreInterfaceName :: InterfaceName
paCoreInterfaceName = "org.PulseAudio.Core1"

paDeviceInterfaceName :: InterfaceName
paDeviceInterfaceName = "org.PulseAudio.Core1.Device"

paServerLookupGenerationParams :: GenerationParams
paServerLookupGenerationParams = defaultGenerationParams
  { genTakeSignalErrorHandler = True
  , genBusName = Just paServerLookupBusName
  , genObjectPath = Just paServerLookupObjectPath
  }

paCoreGenerationParams :: GenerationParams
paCoreGenerationParams = defaultGenerationParams
  { genTakeSignalErrorHandler = True
  , genObjectPath = Just paCoreObjectPath
  }

paDeviceGenerationParams :: GenerationParams
paDeviceGenerationParams = defaultGenerationParams
  { genTakeSignalErrorHandler = True
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
  | BatteryStateCharging
  | BatteryStateDischarging
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
