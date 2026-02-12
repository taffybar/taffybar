{-# LANGUAGE TemplateHaskell #-}

module System.Taffybar.DBus.Client.Login1Manager where

import DBus.Generation
import System.FilePath
import System.Taffybar.DBus.Client.Params
import System.Taffybar.DBus.Client.Util

-- NOTE: The corresponding introspection XML is intentionally minimal (signal-only).
-- The full logind Manager interface contains method/arg names that collide with
-- Haskell reserved words and with generated property setters, which causes TH
-- generation to fail.
generateClientFromFile
  defaultRecordGenerationParams
    { recordName = Just "Login1ManagerInfo",
      recordPrefix = "login1"
    }
  login1GenerationParams {genObjectPath = Just login1ObjectPath}
  False
  $ "dbus-xml" </> "org.freedesktop.login1.Manager.xml"
