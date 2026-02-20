module System.Taffybar.Widget.WindowsSpec (spec) where

import Data.Text qualified as T
import System.Taffybar.Util (truncateText)
import Test.Hspec

spec :: Spec
spec = describe "Windows active label truncation" $ do
  it "truncates raw labels without introducing escaped entities" $ do
    let rawLabel = "Escape &apples"
        truncated = truncateText 8 rawLabel

    truncated `shouldBe` "Escape &…"
    T.isInfixOf "&amp;" truncated `shouldBe` False
