module System.Taffybar.Information.CPUFrequencySpec (spec) where

import System.Taffybar.Information.CPUFrequency
import Test.Hspec

spec :: Spec
spec =
  describe "CPU frequency summaries" $ do
    it "reports average, range, and policy count" $ do
      let info = summarizeCPUFrequencies [1_000_000, 2_000_000, 3_000_000]
      cpuFrequencyAverageKHz info `shouldBe` Just 2_000_000
      cpuFrequencyMinimumKHz info `shouldBe` Just 1_000_000
      cpuFrequencyMaximumKHz info `shouldBe` Just 3_000_000
      cpuFrequencySampleCount info `shouldBe` 3
      cpuFrequencyAverageGHz info `shouldBe` Just 2

    it "represents a missing frequency source without fabricated readings" $ do
      let info = summarizeCPUFrequencies []
      cpuFrequencyAverageKHz info `shouldBe` Nothing
      cpuFrequencyMinimumKHz info `shouldBe` Nothing
      cpuFrequencyMaximumKHz info `shouldBe` Nothing
      cpuFrequencySampleCount info `shouldBe` 0
