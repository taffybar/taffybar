{-# LANGUAGE TemplateHaskell #-}

module System.Taffybar.DBus.Client.ChromeWindowInfo where

import DBus.Generation ()
import System.FilePath
import System.Taffybar.DBus.Client.Params
import System.Taffybar.DBus.Client.Util

generateClientFromFile
  defaultRecordGenerationParams
    { recordName = Just "ChromeWindowInfo",
      recordPrefix = "chromeWindowInfo"
    }
  chromeWindowInfoGenerationParams
  False
  $ "dbus-xml" </> "org.imalison.ChromeWindowInfo.xml"
