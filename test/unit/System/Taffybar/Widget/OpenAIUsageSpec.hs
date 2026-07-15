{-# LANGUAGE OverloadedStrings #-}

module System.Taffybar.Widget.OpenAIUsageSpec (spec) where

import System.Taffybar.Information.OpenAIUsage
import System.Taffybar.Widget.OpenAIUsage
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

weeklyOnlyInfo :: OpenAIUsageInfo
weeklyOnlyInfo = usageInfo weeklyWindow Nothing

normalInfo :: OpenAIUsageInfo
normalInfo = usageInfo fiveHourWindow (Just weeklyWindow)

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

usageWindow :: Int -> Int -> OpenAIUsageWindow
usageWindow usedPercent duration =
  OpenAIUsageWindow
    { openAIUsageUsedPercent = usedPercent,
      openAIUsageWindowDurationSeconds = Just duration,
      openAIUsageResetAfterSeconds = Just duration,
      openAIUsageResetAt = Nothing,
      openAIUsageWindowTotals = Nothing
    }
