{-# LANGUAGE OverloadedStrings #-}

module System.Taffybar.Widget.OpenAIUsageSpec (spec) where

import System.Taffybar.Information.OpenAIUsage
import System.Taffybar.Widget.OpenAIUsage
import System.Taffybar.Widget.Util (UsageWindowLabelParts (..), UsageWindowPosition (..))
import Test.Hspec

spec :: Spec
spec =
  describe "OpenAI usage window labels" $ do
    it "shows an omitted 5-hour limit as unlimited and keeps the 7-day window in the weekly row" $ do
      formatOpenAIUsageWindowLabel OpenAIUsagePrimaryWindow OpenAIUsageDisplayUsed weeklyOnlyInfo
        `shouldBe` "5h ∞"
      formatOpenAIUsageWindowLabel OpenAIUsageSecondaryWindow OpenAIUsageDisplayUsed weeklyOnlyInfo
        `shouldBe` "7d 17%u"
      formatOpenAIUsageSummaryLabel OpenAIUsageDisplayUsed weeklyOnlyInfo
        `shouldBe` "AI 5h ∞ 7d 17%u"

    it "preserves the normal 5-hour and 7-day layout when both windows are present" $ do
      formatOpenAIUsageWindowLabel OpenAIUsagePrimaryWindow OpenAIUsageDisplayRemaining normalInfo
        `shouldBe` "5h 75%r"
      formatOpenAIUsageWindowLabel OpenAIUsageSecondaryWindow OpenAIUsageDisplayRemaining normalInfo
        `shouldBe` "7d 83%r"

    it "shows the current day of seven when the API supplies an authoritative reset time" $ do
      formatOpenAIUsageWindowLabel OpenAIUsageSecondaryWindow OpenAIUsageDisplayRemaining infoWithReset
        `shouldBe` "83%r 3/7d"
      formatOpenAIUsageSummaryLabel OpenAIUsageDisplayRemaining infoWithReset
        `shouldBe` "AI 5h 75%r 83%r 3/7d"

    it "keeps the window day on the weekly row when the 5-hour limit is omitted" $
      formatOpenAIUsageWindowLabel OpenAIUsageSecondaryWindow OpenAIUsageDisplayUsed weeklyOnlyInfoWithReset
        `shouldBe` "17%u 3/7d"

    it "exposes semantic label parts for configurable renderers" $
      openAIUsageWindowLabelParts OpenAIUsageSecondaryWindow OpenAIUsageDisplayRemaining infoWithReset
        `shouldBe` UsageWindowLabelParts "7d" "83%r" (Just $ UsageWindowPosition 3 7)

weeklyOnlyInfo :: OpenAIUsageInfo
weeklyOnlyInfo = usageInfo weeklyWindow Nothing

normalInfo :: OpenAIUsageInfo
normalInfo = usageInfo fiveHourWindow (Just weeklyWindow)

infoWithReset :: OpenAIUsageInfo
infoWithReset = usageInfo fiveHourWindow (Just weeklyWindowWithReset)

weeklyOnlyInfoWithReset :: OpenAIUsageInfo
weeklyOnlyInfoWithReset = usageInfo weeklyWindowWithReset Nothing

usageInfo :: OpenAIUsageWindow -> Maybe OpenAIUsageWindow -> OpenAIUsageInfo
usageInfo primary secondary =
  OpenAIUsageInfo
    { openAIUsagePlanType = Just "pro",
      openAIUsageRateLimit =
        OpenAIUsageRateLimit
          { openAIUsageAllowed = True,
            openAIUsageLimitReached = False,
            openAIUsagePrimaryWindow = Just primary,
            openAIUsageSecondaryWindow = secondary
          },
      openAIUsageAdditionalRateLimits = [],
      openAIUsageCredits = Nothing,
      openAIUsageReachedType = Nothing
    }

fiveHourWindow :: OpenAIUsageWindow
fiveHourWindow = usageWindow 25 (5 * 60 * 60)

weeklyWindow :: OpenAIUsageWindow
weeklyWindow = usageWindow 17 (7 * 24 * 60 * 60)

weeklyWindowWithReset :: OpenAIUsageWindow
weeklyWindowWithReset =
  weeklyWindow
    { openAIUsageResetAfterSeconds = Just (4 * 24 * 60 * 60),
      openAIUsageResetAt = Just $ read "2026-07-22 12:00:00 UTC"
    }

usageWindow :: Int -> Int -> OpenAIUsageWindow
usageWindow usedPercent duration =
  OpenAIUsageWindow
    { openAIUsageUsedPercent = usedPercent,
      openAIUsageWindowDurationSeconds = Just duration,
      openAIUsageResetAfterSeconds = Just duration,
      openAIUsageResetAt = Nothing,
      openAIUsageWindowTotals = Nothing
    }
