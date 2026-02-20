module System.Taffybar.Information.CryptoSpec (spec) where

import System.Taffybar.Information.Crypto
import Test.Hspec

spec :: Spec
spec = describe "Crypto backoff" $ do
  it "caps exponential growth at the configured maximum" $
    nextCryptoBackoff 960 960 `shouldBe` (960, 960)

  it "doubles the current backoff delay when under the maximum" $
    nextCryptoBackoff 960 120 `shouldBe` (240, 120)

  it "sets max backoff to 16x the nominal polling delay" $
    maxCryptoBackoffForDelay 60 `shouldBe` 960
