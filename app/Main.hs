-- | This is just a stub executable that uses dyre to read the config
-- file and recompile itself.
module Main ( main ) where

import System.Taffybar
import System.Taffybar.Context

main :: IO ()
main = dyreTaffybar defaultTaffybarConfig
