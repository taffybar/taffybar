{-# LANGUAGE TemplateHaskell #-}
module System.Taffybar.DBus.Client.PulseAudioCore where

import DBus.Generation ()
import System.FilePath
import System.Taffybar.DBus.Client.Params
import System.Taffybar.DBus.Client.Util

generateClientFromFile
  defaultRecordGenerationParams
  { recordName = Just "PulseAudioCoreInfo"
  , recordPrefix = "pac"
  }
  paCoreGenerationParams
  False $
  "dbus-xml" </> "org.PulseAudio.Core1.xml"
