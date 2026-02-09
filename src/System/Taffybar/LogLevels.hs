{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.LogLevels
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides runtime-configurable log levels for taffybar. It reads
-- a YAML file mapping logger names to log levels, allowing users to enable
-- debug logging for specific modules without recompiling.
--
-- The default file location is @~\/.config\/taffybar\/log-levels.yaml@. The
-- format is a simple YAML mapping:
--
-- > # Enable debug logging for the toggle module
-- > System.Taffybar.DBus.Toggle: DEBUG
-- > Graphics.UI.GIGtkStrut: DEBUG
--
-- Valid log levels are: @DEBUG@, @INFO@, @NOTICE@, @WARNING@, @ERROR@,
-- @CRITICAL@, @ALERT@, @EMERGENCY@.
-----------------------------------------------------------------------------

module System.Taffybar.LogLevels
  ( loadLogLevelsFromFile
  , defaultLogLevelsPath
  ) where

import           Control.Monad (forM_)
import           Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import           System.Directory (doesFileExist)
import           System.Environment.XDG.BaseDir (getUserConfigFile)
import           System.Log.Logger

-- | The default path for the log levels configuration file:
-- @~\/.config\/taffybar\/log-levels.yaml@.
defaultLogLevelsPath :: IO FilePath
defaultLogLevelsPath = getUserConfigFile "taffybar" "log-levels.yaml"

-- | Load log levels from a YAML file. The file should contain a mapping from
-- logger names to log level strings. If the file does not exist, this is a
-- no-op.
--
-- Example YAML:
--
-- > System.Taffybar.DBus.Toggle: DEBUG
-- > System.Taffybar.Context: INFO
loadLogLevelsFromFile :: FilePath -> IO ()
loadLogLevelsFromFile path = do
  exists <- doesFileExist path
  if exists
    then do
      result <- Yaml.decodeFileEither path
      case result of
        Left err ->
          logM loggerName WARNING $
            "Failed to parse " ++ path ++ ": " ++ show err
        Right (Object obj) ->
          forM_ (KM.toList obj) $ \(key, val) ->
            case val of
              String levelStr ->
                case readPriority (T.unpack levelStr) of
                  Just level -> do
                    enableLogger (Key.toString key) level
                    logM loggerName INFO $
                      "Set log level: " ++ Key.toString key ++ " -> " ++ T.unpack levelStr
                  Nothing ->
                    logM loggerName WARNING $
                      "Unknown log level: " ++ T.unpack levelStr
              _ ->
                logM loggerName WARNING $
                  "Expected string value for key: " ++ Key.toString key
        Right _ ->
          logM loggerName WARNING $ "Expected YAML mapping in " ++ path
    else return ()
  where
    loggerName = "System.Taffybar.LogLevels"

enableLogger :: String -> Priority -> IO ()
enableLogger name level = do
  logger <- getLogger name
  saveGlobalLogger $ setLevel level logger

readPriority :: String -> Maybe Priority
readPriority s = case s of
  "DEBUG"     -> Just DEBUG
  "INFO"      -> Just INFO
  "NOTICE"    -> Just NOTICE
  "WARNING"   -> Just WARNING
  "ERROR"     -> Just ERROR
  "CRITICAL"  -> Just CRITICAL
  "ALERT"     -> Just ALERT
  "EMERGENCY" -> Just EMERGENCY
  _           -> Nothing
