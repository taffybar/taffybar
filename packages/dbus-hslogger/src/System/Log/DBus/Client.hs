{-# LANGUAGE TemplateHaskell #-}
module System.Log.DBus.Client where

import DBus.Generation
import System.Log.DBus.Server

generateClient defaultGenerationParams { genObjectPath = Just logPath }
               logIntrospectionInterface
