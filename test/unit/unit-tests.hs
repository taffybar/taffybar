module Main where

import Test.Hspec
import Test.Hspec.Runner

import qualified UnitSpec
import qualified TestLibSpec

main :: IO ()
main = hspecWith defaultConfig $ do
  UnitSpec.spec
  describe "testlib Sanity Checks" TestLibSpec.spec
