{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.Information.DBusObjects where

import DBus.Generation

playerGenerationParams :: GenerationParams
playerGenerationParams =
  defaultGenerationParams
  { genTakeSignalErrorHandler = True
  , genObjectPath = Just "/org/mpris/MediaPlayer2"
  }


