{-# LANGUAGE TemplateHaskell #-}
module System.Information.DBusClients where

import DBus.Generation
import System.Information.DBusObjects

generateClient playerGenerationParams playerInterface
generateSignalsFromInterface playerGenerationParams playerInterface
