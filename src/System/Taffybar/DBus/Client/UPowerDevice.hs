{-# LANGUAGE TemplateHaskell #-}

-- | Generated DBus client bindings for @org.freedesktop.UPower.Device@.
module System.Taffybar.DBus.Client.UPowerDevice where

import System.FilePath
import System.Taffybar.DBus.Client.Params
import System.Taffybar.DBus.Client.Util

-- | Generate the UPower device client and the 'BatteryInfo' record type.
generateClientFromFile
  defaultRecordGenerationParams
    { recordName = Just "BatteryInfo",
      recordPrefix = "battery",
      recordTypeForName = batteryTypeForName
    }
  uPowerGenerationParams
  False
  $ "dbus-xml" </> "org.freedesktop.UPower.Device.xml"
