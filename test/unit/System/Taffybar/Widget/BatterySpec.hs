{-# LANGUAGE OverloadedStrings #-}

module System.Taffybar.Widget.BatterySpec (spec) where

import DBus (toVariant)
import Data.Map qualified as M
import Data.Word (Word32)
import System.Taffybar.Information.Battery
import System.Taffybar.Widget.Battery
import Test.Hspec

spec :: Spec
spec = do
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

    it "uses positive watts for charging and negative watts for discharging" $ do
      let info state rate =
            infoMapToBatteryInfo $
              M.fromList
                [ ("State", toVariant (state :: Word32)),
                  ("EnergyRate", toVariant (rate :: Double))
                ]
      formatBatteryInfo (info 1 19.966) "$signedWatts$W"
        `shouldBe` "+20.0W"
      formatBatteryInfo (info 2 12.34) "$signedWatts$W"
        `shouldBe` "-12.3W"

  describe "defaultMonitorDisplayBatteryProperties" $
    it "refreshes on UPower sampling updates" $
      defaultMonitorDisplayBatteryProperties
        `shouldContain` ["EnergyRate", "UpdateTime"]
