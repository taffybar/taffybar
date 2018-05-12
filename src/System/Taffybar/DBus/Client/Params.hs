{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.DBus.Client.Params where

import DBus.Generation

playerGenerationParams :: GenerationParams
playerGenerationParams = defaultGenerationParams
  { genTakeSignalErrorHandler = True
  , genObjectPath = Just "/org/mpris/MediaPlayer2"
  }

uPowerGenerationParams :: GenerationParams
uPowerGenerationParams = defaultGenerationParams
  { genTakeSignalErrorHandler = True
  , genObjectPath = Just "/org/freedesktop/UPower"
  }
