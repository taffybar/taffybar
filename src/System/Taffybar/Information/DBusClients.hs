{-# LANGUAGE TemplateHaskell #-}
module System.Taffybar.Information.DBusClients where

import StatusNotifier.Util
import System.FilePath
import System.Taffybar.Information.DBusObjects

generateClientFromFile playerGenerationParams False $
                       "dbus-xml" </> "org.mpris.MediaPlayer2.xml"

generateClientFromFile playerGenerationParams False $
                       "dbus-xml" </> "org.mpris.MediaPlayer2.Player.xml"
