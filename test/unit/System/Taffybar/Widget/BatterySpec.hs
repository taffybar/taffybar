{-# LANGUAGE OverloadedStrings #-}

module System.Taffybar.Widget.BatterySpec (spec) where

import DBus (toVariant)
import Data.Map qualified as M
import System.Taffybar.Information.Battery
import System.Taffybar.Widget.Battery
import Test.Hspec

spec :: Spec
spec =
  describe "formatBatteryInfo" $ do
    it "formats the current energy rate in watts" $ do
      let info =
            infoMapToBatteryInfo $
              M.fromList
                [ ("Percentage", toVariant (96.0 :: Double)),
                  ("EnergyRate", toVariant (19.966 :: Double))
                ]
      formatBatteryInfo info "$percentage$% $watts$W"
        `shouldBe` "96% 20.0W"
