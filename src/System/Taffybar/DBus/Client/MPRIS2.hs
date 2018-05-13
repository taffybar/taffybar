{-# LANGUAGE TemplateHaskell #-}
module System.Taffybar.DBus.Client.MPRIS2 where

import System.Taffybar.DBus.Client.Util
import System.FilePath
import System.Taffybar.DBus.Client.Params

generateClientFromFile defaultRecordGenerationParams playerGenerationParams False $
                       "dbus-xml" </> "org.mpris.MediaPlayer2.xml"

generateClientFromFile defaultRecordGenerationParams playerGenerationParams False $
                       "dbus-xml" </> "org.mpris.MediaPlayer2.Player.xml"
