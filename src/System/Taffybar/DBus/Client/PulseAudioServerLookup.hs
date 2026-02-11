{-# LANGUAGE TemplateHaskell #-}

module System.Taffybar.DBus.Client.PulseAudioServerLookup where

import DBus.Generation ()
import System.FilePath
import System.Taffybar.DBus.Client.Params
import System.Taffybar.DBus.Client.Util

generateClientFromFile
  defaultRecordGenerationParams
    { recordName = Just "PulseAudioServerLookupInfo",
      recordPrefix = "pasl"
    }
  paServerLookupGenerationParams
  False
  $ "dbus-xml" </> "org.PulseAudio.ServerLookup1.xml"
