module System.Taffybar.Information.ASUSSpec (spec) where

import System.Taffybar.Information.ASUS
import Test.Hspec

spec :: Spec
spec = describe "ASUS platform profile DBus encoding" $ do
  it "decodes the asusd 6 profile values" $
    map asusProfileFromUInt [0, 1, 2, 99]
      `shouldBe` [Just Balanced, Just Performance, Just Quiet, Nothing]

  it "encodes the asusd 6 profile values" $
    map asusProfileToUInt [Balanced, Performance, Quiet]
      `shouldBe` [0, 1, 2]
