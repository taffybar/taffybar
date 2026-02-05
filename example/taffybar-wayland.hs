{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Taffybar
import System.Taffybar.Example

main :: IO ()
main = startTaffybar exampleWaylandTaffybarConfig
