{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This is just a stub executable that uses dyre to read the config file and
-- recompile itself.
module Main (main) where

import Data.Default (def)
import Data.Semigroup ((<>))
import Data.Version
import Options.Applicative
import Paths_taffybar (version)
import System.Directory
import System.Log.Logger
import System.Taffybar
import System.Taffybar.Context
import System.Taffybar.Example
import Text.Printf

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

main :: IO ()
main = do
  logLevel <-
    execParser $
      info
        (helper <*> versionOption <*> logP)
        ( fullDesc
            <> progDesc "Start taffybar, recompiling if necessary"
        )

  logger <- getLogger "System.Taffybar"
  saveGlobalLogger $ setLevel logLevel logger

  taffyFilepath <- getTaffyFile "taffybar.hs"
  configurationExists <- doesFileExist taffyFilepath

  if configurationExists
    -- XXX: The configuration record here does not get used, this just calls in to dyre.
    then dyreTaffybar def
    else do
      detectedBackend <- detectBackend
      let exampleConfig = case detectedBackend of
            BackendWayland -> exampleWaylandTaffybarConfig
            BackendX11 -> exampleTaffybarConfig
      logM
        "System.Taffybar"
        WARNING
        ( printf "No taffybar configuration file found at %s." taffyFilepath
            ++ " Starting with example configuration for "
            ++ show detectedBackend
            ++ "."
        )
      startTaffybar exampleConfig
