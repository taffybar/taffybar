{-# LANGUAGE TemplateHaskell #-}
module System.Taffybar.DBus.Client.MPRIS2 where

import StatusNotifier.Util
import System.FilePath
import System.Taffybar.DBus.Client.Params

generateClientFromFile playerGenerationParams False $
                       "dbus-xml" </> "org.mpris.MediaPlayer2.xml"

generateClientFromFile playerGenerationParams False $
                       "dbus-xml" </> "org.mpris.MediaPlayer2.Player.xml"
