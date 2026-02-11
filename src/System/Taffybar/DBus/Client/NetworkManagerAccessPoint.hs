{-# LANGUAGE TemplateHaskell #-}

module System.Taffybar.DBus.Client.NetworkManagerAccessPoint where

import System.FilePath
import System.Taffybar.DBus.Client.Params
import System.Taffybar.DBus.Client.Util

generateClientFromFile
  defaultRecordGenerationParams
    { recordName = Just "AccessPointInfo",
      recordPrefix = "nmap"
    }
  nmGenerationParams
  False
  $ "dbus-xml" </> "org.freedesktop.NetworkManager.AccessPoint.xml"
