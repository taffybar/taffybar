{-# LANGUAGE TemplateHaskell #-}

module System.Taffybar.DBus.Client.UPower where

import DBus.Generation
import System.FilePath
import System.Taffybar.DBus.Client.Params
import System.Taffybar.DBus.Client.Util

generateClientFromFile
  defaultRecordGenerationParams
    { recordName = Just "UPowerInfo",
      recordPrefix = "upi"
    }
  uPowerGenerationParams {genObjectPath = Just uPowerBaseObjectPath}
  False
  $ "dbus-xml" </> "org.freedesktop.UPower.xml"
