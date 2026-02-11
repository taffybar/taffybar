{-# LANGUAGE TemplateHaskell #-}

module System.Taffybar.DBus.Client.MPRIS2 where

import System.FilePath
import System.Taffybar.DBus.Client.Params
import System.Taffybar.DBus.Client.Util

generateClientFromFile defaultRecordGenerationParams playerGenerationParams False $
  "dbus-xml" </> "org.mpris.MediaPlayer2.xml"

generateClientFromFile defaultRecordGenerationParams playerGenerationParams False $
  "dbus-xml" </> "org.mpris.MediaPlayer2.Player.xml"
