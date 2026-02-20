{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This is just a stub executable that uses dyre to read the config file and
-- recompile itself.
module Main (main) where

import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Version
import DhallConfig (runDhallConfigFromFile)
import Options.Applicative
import Paths_taffybar (version)
import System.Directory
import System.Exit (exitFailure)
import System.Log.Logger
import System.Taffybar
import System.Taffybar.Context
import System.Taffybar.Example
import Text.Printf

data StartupMode
  = StartupAuto
  | StartupDyre
  | StartupExample
  | StartupDhall
  deriving (Eq, Show)

data Options = Options
  { optsLogLevel :: Priority,
    optsStartupMode :: StartupMode
  }

logP :: Parser Priority
logP =
  option
    auto
    ( long "log-level"
        <> short 'l'
        <> help "Set the log level"
        <> metavar "LEVEL"
        <> value WARNING
    )

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    (printf "taffybar %s" $ showVersion version)
    ( long "version"
        <> help "Show the version number of taffybar"
    )

startupModeP :: Parser StartupMode
startupModeP =
  fromMaybe StartupAuto
    <$> optional
      ( hsubparser
          ( command
              "auto"
              (info (pure StartupAuto) (progDesc "Use config autodetection (default)"))
              <> command
                "dyre"
                (info (pure StartupDyre) (progDesc "Run dyre with ~/.config/taffybar/taffybar.hs"))
              <> command
                "example"
                (info (pure StartupExample) (progDesc "Run built-in example configuration"))
              <> command
                "dhall"
                (info (pure StartupDhall) (progDesc "Run ~/.config/taffybar/taffybar.dhall"))
          )
      )

optionsP :: Parser Options
optionsP =
  Options
    <$> logP
    <*> startupModeP

runExampleForDetectedBackend :: IO ()
runExampleForDetectedBackend = do
  detectedBackend <- detectBackend
  let exampleConfig = case detectedBackend of
        BackendWayland -> exampleWaylandTaffybarConfig
        BackendX11 -> exampleTaffybarConfig
  logM
    "System.Taffybar"
    WARNING
    (printf "Starting with example configuration for %s." (show detectedBackend))
  startTaffybar exampleConfig

runDhall :: IO ()
runDhall = do
  dhallFilepath <- getTaffyFile "taffybar.dhall"
  dhallExists <- doesFileExist dhallFilepath
  if dhallExists
    then runDhallConfigFromFile dhallFilepath
    else do
      logM
        "System.Taffybar"
        ERROR
        ( printf
            "Dhall mode selected, but %s does not exist."
            dhallFilepath
        )
      exitFailure

runAuto :: IO ()
runAuto = do
  taffyFilepath <- getTaffyFile "taffybar.hs"
  dhallFilepath <- getTaffyFile "taffybar.dhall"
  hasHaskellConfig <- doesFileExist taffyFilepath
  hasDhallConfig <- doesFileExist dhallFilepath

  if hasHaskellConfig
    -- XXX: The configuration record here does not get used, this just calls in to dyre.
    then dyreTaffybar def
    else
      if hasDhallConfig
        then runDhall
        else do
          logM
            "System.Taffybar"
            WARNING
            ( printf "No taffybar configuration file found at %s or %s." taffyFilepath dhallFilepath
                ++ " Falling back to built-in example configuration."
            )
          runExampleForDetectedBackend

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (helper <*> versionOption <*> optionsP)
        ( fullDesc
            <> progDesc "Start taffybar"
        )
  let logLevel = optsLogLevel opts
      startupMode = optsStartupMode opts

  logger <- getLogger "System.Taffybar"
  saveGlobalLogger $ setLevel logLevel logger

  case startupMode of
    StartupAuto -> runAuto
    StartupDyre -> dyreTaffybar def
    StartupExample -> runExampleForDetectedBackend
    StartupDhall -> runDhall
