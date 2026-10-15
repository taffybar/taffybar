{-# LANGUAGE OverloadedStrings #-}

module System.Taffybar.Widget.BatterySpec (spec) where

import DBus (toVariant)
import Data.List (mapAccumL)
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

  describe "battery energy flow" $ do
    let sample timestamp energy =
          (infoMapToBatteryInfo M.empty)
            { batteryState = BatteryStateCharging,
              batteryIsPresent = True,
              batteryEnergy = energy,
              batteryEnergyFull = 66.757,
              batteryEnergyRate = 40,
              batteryUpdateTime = timestamp,
              batteryIconName = "battery-full-charging-symbolic",
              batteryTimeToFull = 1200
            }
        reconcile = snd . mapAccumL step Nothing
          where
            step previous current =
              let (history, corrected) = reconcileBatteryEnergy previous current
               in (Just history, corrected)
        watts = map (`formatBatteryInfo` "$signedWatts$W") . reconcile

    it "shows net discharge while USB firmware reports charging" $ do
      let [_, corrected] = reconcile [sample 100 54.074, sample 130 53.406]
      formatBatteryInfo corrected "$signedWatts$W" `shouldBe` "-40.0W"
      batteryState corrected `shouldBe` BatteryStateDischarging
      batteryIconName corrected `shouldBe` "battery-full-symbolic"
      batteryTimeToFull corrected `shouldBe` 0
      batteryTimeToEmpty corrected `shouldBe` 4807

    it "retains direction between quantized energy updates" $
      watts [sample 100 54.074, sample 130 53.406, sample 160 53.406]
        `shouldBe` ["+40.0W", "-40.0W", "-40.0W"]

    it "switches back when net charging actually starts" $
      watts [sample 100 54.074, sample 130 53.406, sample 160 54.074]
        `shouldBe` ["+40.0W", "-40.0W", "+40.0W"]

    it "also corrects a discharging flag when stored energy increases" $ do
      let samples =
            map
              (\info -> info {batteryState = BatteryStateDischarging, batteryIconName = "battery-full-symbolic"})
              [sample 100 53.406, sample 130 54.074]
          corrected = last $ reconcile samples
      formatBatteryInfo corrected "$signedWatts$W" `shouldBe` "+40.0W"
      batteryIconName corrected `shouldBe` "battery-full-charging-symbolic"
      batteryTimeToEmpty corrected `shouldBe` 0
      batteryTimeToFull corrected `shouldBe` 1141

    it "resets on reported state changes such as unplugging and reconnecting" $
      watts
        [ sample 100 54.074,
          sample 130 53.406,
          (sample 140 53.406) {batteryState = BatteryStateDischarging},
          sample 150 53.406
        ]
        `shouldBe` ["+40.0W", "-40.0W", "-40.0W", "+40.0W"]

    it "does not infer direction across a long sampling gap or clock reversal" $ do
      watts [sample 100 54.074, sample 300 53.406]
        `shouldBe` ["+40.0W", "+40.0W"]
      watts [sample 100 54.074, sample 130 53.406, sample 120 52.739]
        `shouldBe` ["+40.0W", "-40.0W", "+40.0W"]

    it "expires old evidence when the energy stops changing" $
      watts [sample 100 54.074, sample 130 53.406, sample 220 53.406, sample 280 53.406]
        `shouldBe` ["+40.0W", "-40.0W", "-40.0W", "+40.0W"]

    it "ignores duplicate timestamps and resets on battery replacement" $ do
      watts [sample 100 54.074, sample 100 53.406, sample 130 53.406]
        `shouldBe` ["+40.0W", "+40.0W", "-40.0W"]
      watts [sample 100 54.074, sample 130 53.406, (sample 160 52) {batteryEnergyFull = 60}]
        `shouldBe` ["+40.0W", "-40.0W", "+40.0W"]

    it "leaves full, absent, and missing energy readings alone" $ do
      let full = (sample 160 53.406) {batteryState = BatteryStateFullyCharged, batteryEnergyRate = 0}
      watts [sample 100 54.074, sample 130 53.406, full]
        `shouldBe` ["+40.0W", "-40.0W", "0.0W"]
      watts [sample 100 54.074, (sample 130 0) {batteryIsPresent = False}]
        `shouldBe` ["+40.0W", "+40.0W"]
      watts (map (\info -> info {batteryEnergyFull = 0}) [sample 100 54.074, sample 130 53.406])
        `shouldBe` ["+40.0W", "+40.0W"]
      watts [sample 100 54.074, sample 130 53.406, (sample 160 53.406) {batteryEnergyRate = 0}]
        `shouldBe` ["+40.0W", "-40.0W", "0.0W"]
