{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE ApplicativeDo #-}
-- | This is just a stub executable that uses dyre to read the config file and
-- recompile itself.
module Main ( main ) where

import Data.Default (def)
import Data.Semigroup ((<>))
import Data.Version
import Options.Applicative
import System.Directory
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

versionOption :: Parser (a -> a)
versionOption = infoOption
                (printf "taffybar %s" $ showVersion version)
                (  long "version"
                <> help "Show the version number of taffybar"
                )

configurationFilePathOption :: FilePath -> Parser FilePath
configurationFilePathOption defaultFilePath =
  option auto
  (  long "config-file"
  <> short 'c'
  <> help "Set the path that should be used to load taffybar"
  <> metavar "CONFIGURATION-FILEPATH"
  <> value defaultFilePath
  )

makeParser :: FilePath -> Parser (FilePath, Priority)
makeParser defaultFilePath = do
  filepath <- configurationFilePathOption defaultFilePath
  logLevel <- logP
  return (filepath, logLevel)


main :: IO ()
main = do
  defaultFilePath <- getTaffyFile "taffybar.hs"
  (configFilePath, logLevel) <-
    execParser $ info (helper <*> versionOption <*> makeParser defaultFilePath)
              (  fullDesc
              <> progDesc "Start taffybar, recompiling if necessary"
              )

  logger <- getLogger "System.Taffybar"
  saveGlobalLogger $ setLevel logLevel logger

  configurationExists <- doesFileExist configFilePath

  if configurationExists
  -- XXX: The configuration record here does not get used, this just calls in to dyre.
  then dyreTaffybar def
  else do
    logM "System.Taffybar" WARNING $
           (  printf "No taffybar configuration file found at %s." configFilePath
           ++ " Starting with example configuration."
           )
    startTaffybar exampleTaffybarConfig
