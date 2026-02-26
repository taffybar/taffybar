{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import DBus
import DBus.Client
import Data.Semigroup ((<>))
import Options.Applicative
import System.Log.DBus.Client

levelP :: Parser String
levelP =
  strOption
  (  long "level"
  <> short 'l'
  <> help "The level to which to set the log"
  <> metavar "LEVEL"
  <> value "INFO"
  )

prefixP :: Parser String
prefixP =
  strOption
  (  long "prefix"
  <> short 'p'
  <> help "The log prefix whose level will be set"
  <> metavar "PREFIX"
  <> value "System.Taffybar"
  )

busNameP :: Parser BusName
busNameP = busName_ <$>
  strOption
  (  long "bus-name"
  <> short 'b'
  <> help "The bus name to which the message should be sent"
  <> value "org.taffybar.Bar"
  )

doSetLogLevel :: Client -> Parser (IO (Either MethodError ()))
doSetLogLevel client = setLogLevel client <$> busNameP <*> prefixP <*> levelP

main = do
  client <- connectSession
  res <- join $ execParser $
         info (doSetLogLevel client <**> helper)
              (  fullDesc
              <> progDesc "Set the log level of a running process")
  print res
