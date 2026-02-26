module Main (main) where

import Test.Hspec

import qualified HostSpec
import qualified UtilSpec
import qualified WatcherSpec

main :: IO ()
main = hspec $ do
  UtilSpec.spec
  WatcherSpec.spec
  HostSpec.spec
