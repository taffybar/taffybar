-- | This is just a stub executable that uses dyre to read the config file and
-- recompile itself.
module Main ( main ) where

import Data.Semigroup ((<>))
import Data.Version
import Options.Applicative
import System.Log.Logger
import System.Taffybar
import System.Taffybar.Context
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

main :: IO ()
main = do
  logLevel <- execParser $ info (helper <*> versionOption <*> logP)
              (  fullDesc
              <> progDesc "Start taffybar, recompiling if necessary"
              )
  logger <- getLogger "System.Taffybar"
  saveGlobalLogger $ setLevel logLevel logger
  dyreTaffybar defaultTaffybarConfig
