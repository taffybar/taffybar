{-# LANGUAGE TemplateHaskell #-}

module System.Taffybar.DBus.Client.NetworkManager where

import DBus.Generation
import System.FilePath
import System.Taffybar.DBus.Client.Params
import System.Taffybar.DBus.Client.Util

generateClientFromFile
  defaultRecordGenerationParams
    { recordName = Just "NetworkManagerInfo",
      recordPrefix = "nm"
    }
  nmGenerationParams {genObjectPath = Just nmObjectPath}
  False
  $ "dbus-xml" </> "org.freedesktop.NetworkManager.xml"
