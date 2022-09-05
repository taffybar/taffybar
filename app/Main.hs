{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- | This is just a stub executable that uses dyre to read the config file and
-- recompile itself.
module Main ( main ) where

import qualified Config.Dyre as Dyre
import Data.Default (def)
import Data.Semigroup ((<>))
import Data.Version
import Options.Applicative
import System.Directory
import System.Environment.XDG.BaseDir
import System.Log.Logger
import System.Taffybar
import System.Taffybar.Context
import System.Taffybar.Example
import Text.Printf

import Paths_taffybar (version)

logP :: Parser Priority
logP =
  option auto
  (  long "log-level"
  <> short 'l'
  <> help "Set the log level"
  <> metavar "LEVEL"
  <> value WARNING
  )

projectNameP :: Parser String
projectNameP =
  option str
  (  long "project-name"
  <> short 'p'
  <> help "Set taffybar configuration project name"
  <> metavar "NAME"
  <> value "taffybar"
  )

versionOption :: Parser (a -> a)
versionOption = infoOption
                (printf "taffybar %s" $ showVersion version)
                (  long "version"
                <> help "Show the version number of taffybar"
                )

main :: IO ()
main = do
  (logLevel, projectName) <-
    execParser $ info (helper <*> versionOption <*> ((,) <$> logP <*> projectNameP))
              (  fullDesc
              <> progDesc "Start taffybar, recompiling if necessary"
              )

  logger <- getLogger "System.Taffybar"
  saveGlobalLogger $ setLevel logLevel logger

  taffyFilepath <- getUserConfigFile projectName $ projectName <> ".hs"
  configurationExists <- doesFileExist taffyFilepath

  if configurationExists
  -- XXX: The configuration record here does not get used, this just calls in to dyre.
  then Dyre.wrapMain (taffybarDyreParams {Dyre.projectName = projectName}) def
  else do
    logM "System.Taffybar" WARNING $
           (  printf "No taffybar configuration file found at %s." taffyFilepath
           ++ " Starting with example configuration."
           )
    startTaffybar exampleTaffybarConfig
