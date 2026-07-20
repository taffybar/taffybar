module System.Taffybar.Information.CPUPowerSpec (spec) where

import System.Taffybar.Information.CPUPower
import Test.Hspec

spec :: Spec
spec =
  describe "CPU package power calculation" $ do
    it "converts an energy delta over time to watts" $ do
      let previous = EnergySample 10_000_000 1_000_000_000
          current = EnergySample 55_000_000 2_000_000_000
      calculateCPUPowerWatts 100_000_000 previous current `shouldBe` Just 45

    it "accounts for a wrapping energy counter" $ do
      let previous = EnergySample 95_000_000 1_000_000_000
          current = EnergySample 5_000_000 2_000_000_000
      calculateCPUPowerWatts 100_000_000 previous current `shouldBe` Just 10

    it "rejects samples without elapsed time" $ do
      let previous = EnergySample 10_000_000 1_000_000_000
          current = EnergySample 20_000_000 1_000_000_000
      calculateCPUPowerWatts 100_000_000 previous current `shouldBe` Nothing
