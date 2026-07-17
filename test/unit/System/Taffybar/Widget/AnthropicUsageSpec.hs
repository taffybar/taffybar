{-# LANGUAGE OverloadedStrings #-}

module System.Taffybar.Widget.AnthropicUsageSpec (spec) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import System.Taffybar.Information.AnthropicUsage
import System.Taffybar.Widget.AnthropicUsage
import Test.Hspec

spec :: Spec
spec =
  describe "Anthropic usage weekly window label" $ do
    it "shows the current day of seven when the OAuth endpoint supplies a reset time" $
      formatAnthropicUsageWindowLabel AnthropicUsageWeeklyWindow AnthropicUsageDisplayRemaining infoWithReset
        `shouldBe` "7d 35%·F35%r 3/7"

    it "uses the ceiling of elapsed 24-hour periods at an exact day boundary" $
      formatAnthropicUsageWindowLabel AnthropicUsageWeeklyWindow AnthropicUsageDisplayRemaining infoAtOneDay
        `shouldBe` "7d 35%r 1/7"

    it "omits the window day for a synthesized transcript fallback window" $
      formatAnthropicUsageWindowLabel AnthropicUsageWeeklyWindow AnthropicUsageDisplayRemaining infoWithoutReset
        `shouldBe` "7d 35%r"

infoWithReset :: AnthropicUsageInfo
infoWithReset =
  (usageInfo generatedAt $ Just resetAt)
    { anthropicUsageScopedWeeklyWindow = Just $ usageWindow "Fable" Nothing
    }

infoAtOneDay :: AnthropicUsageInfo
infoAtOneDay =
  usageInfo
    (read "2026-07-13 12:00:00 UTC")
    (Just resetAt)

infoWithoutReset :: AnthropicUsageInfo
infoWithoutReset = usageInfo generatedAt Nothing

usageInfo :: UTCTime -> Maybe UTCTime -> AnthropicUsageInfo
usageInfo generated reset =
  AnthropicUsageInfo
    { anthropicUsageGeneratedAt = generated,
      anthropicUsageSubscriptionType = Just "max",
      anthropicUsageRateLimitTier = Nothing,
      anthropicUsageHasAvailableSubscription = Just True,
      anthropicUsageExtraUsageDisabledReason = Nothing,
      anthropicUsageOrganizationName = Nothing,
      anthropicUsageFiveHourWindow = usageWindow "5h" Nothing,
      anthropicUsageWeeklyWindow = usageWindow "7d" reset,
      anthropicUsageScopedWeeklyWindow = Nothing
    }

generatedAt :: UTCTime
generatedAt = read "2026-07-15 00:00:00 UTC"

resetAt :: UTCTime
resetAt = read "2026-07-19 12:00:00 UTC"

usageWindow :: Text -> Maybe UTCTime -> AnthropicUsageWindow
usageWindow name resetTime =
  AnthropicUsageWindow
    { anthropicUsageWindowName = name,
      anthropicUsageWindowStart = read "2026-07-12 12:00:00 UTC",
      anthropicUsageWindowEnd = maybe (read "2026-07-19 12:00:00 UTC") id resetTime,
      anthropicUsageWindowResetAt = resetTime,
      anthropicUsageWindowBudgetTokens = Nothing,
      anthropicUsageWindowUtilizationPercent = Just 65,
      anthropicUsageWindowTotals = mempty
    }
