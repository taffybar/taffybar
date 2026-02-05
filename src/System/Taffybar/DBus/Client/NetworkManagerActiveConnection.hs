{-# LANGUAGE TemplateHaskell #-}
module System.Taffybar.DBus.Client.NetworkManagerActiveConnection where

import System.FilePath
import System.Taffybar.DBus.Client.Params
import System.Taffybar.DBus.Client.Util

generateClientFromFile
  defaultRecordGenerationParams
  { recordName = Just "ActiveConnectionInfo"
  , recordPrefix = "nmac"
  }
  nmGenerationParams
  False $
  "dbus-xml" </> "org.freedesktop.NetworkManager.Connection.Active.xml"
