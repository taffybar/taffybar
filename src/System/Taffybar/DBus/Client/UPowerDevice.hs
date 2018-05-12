{-# LANGUAGE TemplateHaskell #-}
module System.Taffybar.DBus.Client.UPowerDevice where

import StatusNotifier.Util
import System.FilePath
import System.Taffybar.DBus.Client.Params

generateClientFromFile uPowerGenerationParams False $
                       "dbus-xml" </> "org.freedesktop.UPower.Device.xml"
