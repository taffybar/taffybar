module Main where

import Test.Hspec
import Test.Hspec.Runner
import TestLibSpec qualified
import UnitSpec qualified

main :: IO ()
main = hspecWith defaultConfig $ do
  UnitSpec.spec
  describe "testlib Sanity Checks" TestLibSpec.spec
