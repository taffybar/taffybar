{-# LANGUAGE TemplateHaskell #-}
module System.Taffybar.DBus.Client.PulseAudioDevice where

import DBus.Generation ()
import System.FilePath
import System.Taffybar.DBus.Client.Params
import System.Taffybar.DBus.Client.Util

generateClientFromFile
  defaultRecordGenerationParams
  { recordName = Just "PulseAudioDeviceInfo"
  , recordPrefix = "pad"
  }
  paDeviceGenerationParams
  False $
  "dbus-xml" </> "org.PulseAudio.Core1.Device.xml"
