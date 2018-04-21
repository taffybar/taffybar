{-# LANGUAGE OverloadedStrings #-}
module System.Information.DBusObjects where

import System.IO.Unsafe
import DBus.Generation
import DBus.Introspection
import Data.Maybe

{-# NOINLINE playerObject #-}
playerObject :: Object
playerObject = unsafePerformIO $
  head . maybeToList . parseXML "/" <$>
  readFile "dbus-xml/org.mpris.MediaPlayer2.Player.xml"

playerInterface :: Interface
playerInterface =
  head $ objectInterfaces playerObject

playerGenerationParams :: GenerationParams
playerGenerationParams =
  defaultGenerationParams
  { genTakeSignalErrorHandler = True
  , genObjectPath = Just "/org/mpris/MediaPlayer2"
  }
