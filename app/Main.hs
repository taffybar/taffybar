-- | This is just a stub executable that uses dyre to read the config
-- file and recompile itself.
module Main ( main ) where

import System.Taffybar

main :: IO ()
main = do
  defaultTaffybar defaultTaffybarConfig
