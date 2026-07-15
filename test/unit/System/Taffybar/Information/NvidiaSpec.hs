{-# LANGUAGE OverloadedStrings #-}

module System.Taffybar.Information.NvidiaSpec (spec) where

import System.Taffybar.Information.Nvidia
import Test.Hspec

spec :: Spec
spec = describe "NVIDIA temperature parsing" $ do
  it "parses and sorts nvidia-smi temperature rows" $
    parseNvidiaGpuTemperatures "2, 73\n0, 56\n"
      `shouldBe` [ NvidiaGpuTemperature 0 56,
                   NvidiaGpuTemperature 2 73
                 ]

  it "ignores malformed and unavailable rows" $
    parseNvidiaGpuTemperatures "0, N/A\nbad row\n1, 64\n"
      `shouldBe` [NvidiaGpuTemperature 1 64]
