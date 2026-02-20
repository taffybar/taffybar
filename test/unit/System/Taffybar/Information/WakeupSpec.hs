{-# LANGUAGE NumericUnderscores #-}

module System.Taffybar.Information.WakeupSpec (spec) where

import Data.Word (Word64)
import System.Taffybar.Information.Wakeup
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "nextWallAlignedWakeupNs" $ do
    it "aligns 1-second intervals to top-of-second boundaries" $ do
      let realtimeOffset = 0
          interval = 1_000_000_000
          now = 1_250_000_000
      nextWallAlignedWakeupNs realtimeOffset interval now
        `shouldBe` 2_000_000_000

    prop "returns a strict future boundary aligned to wall-clock phase" $
      \(Positive (Small intervalSeconds :: Small Int))
       (NonNegative (Small nowOffsetMicros :: Small Int))
       (Small offsetMicros :: Small Int) ->
          let anchor :: Word64
              anchor = 1_000_000_000
              interval :: Word64
              interval = fromIntegral intervalSeconds * 1_000_000_000
              now :: Word64
              now = anchor + fromIntegral nowOffsetMicros * 1_000
              realtimeOffsetNs :: Integer
              realtimeOffsetNs = fromIntegral offsetMicros * 1_000
              due = nextWallAlignedWakeupNs realtimeOffsetNs interval now
           in due > now
                && (toInteger due + realtimeOffsetNs) `mod` toInteger interval == 0

  describe "nextAlignedWakeupNs" $ do
    it "returns the next boundary when now is exactly on a boundary" $ do
      let anchor = 1_000_000_000
          interval = 5_000_000_000
          now = 6_000_000_000
      nextAlignedWakeupNs anchor interval now
        `shouldBe` 11_000_000_000

    prop "returns a strict future boundary aligned to the interval" $
      \(Positive (Small intervalSeconds :: Small Int)) (NonNegative (Small nowOffsetMicros :: Small Int)) ->
        let anchor :: Word64
            anchor = 1_000_000_000
            interval :: Word64
            interval = fromIntegral intervalSeconds * 1_000_000_000
            now :: Word64
            now = anchor + fromIntegral nowOffsetMicros * 1_000
            due = nextAlignedWakeupNs anchor interval now
         in due > now
              && (due - anchor) `mod` interval == 0

  describe "divisibility alignment" $ do
    it "keeps 10s wakeups synchronized to every other 5s wakeup" $ do
      let anchor :: Word64
          anchor = 0
          fiveSeconds :: Word64
          fiveSeconds = 5_000_000_000
          tenSeconds :: Word64
          tenSeconds = 10_000_000_000
          fiveSchedule = map (intervalDueAtStepNs anchor fiveSeconds) [1 .. 10]
          tenSchedule = map (intervalDueAtStepNs anchor tenSeconds) [1 .. 5]
          everyOtherFive = [fiveSchedule !! 1, fiveSchedule !! 3, fiveSchedule !! 5, fiveSchedule !! 7, fiveSchedule !! 9]
      tenSchedule `shouldBe` everyOtherFive

  describe "minute alignment" $ do
    it "lands on exact minute boundaries for intervals that divide 60 seconds" $ do
      let minuteNs :: Word64
          minuteNs = 60_000_000_000
          divisorIntervalsSeconds :: [Word64]
          divisorIntervalsSeconds = [1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60]
          checkInterval intervalSeconds =
            let intervalNs = intervalSeconds * 1_000_000_000
                stepAtMinute = 60 `div` intervalSeconds
             in intervalDueAtStepNs 0 intervalNs stepAtMinute == minuteNs
      map checkInterval divisorIntervalsSeconds `shouldBe` replicate (length divisorIntervalsSeconds) True
