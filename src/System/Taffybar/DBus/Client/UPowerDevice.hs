{-# LANGUAGE TemplateHaskell #-}

module System.Taffybar.DBus.Client.UPowerDevice where

import System.FilePath
import System.Taffybar.DBus.Client.Params
import System.Taffybar.DBus.Client.Util

generateClientFromFile
  defaultRecordGenerationParams
    { recordName = Just "BatteryInfo",
      recordPrefix = "battery",
      recordTypeForName = batteryTypeForName
    }
  uPowerGenerationParams
  False
  $ "dbus-xml" </> "org.freedesktop.UPower.Device.xml"
