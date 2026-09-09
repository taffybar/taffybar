{-# LANGUAGE OverloadedStrings #-}

module System.Taffybar.Widget.NetworkGraphSpec (spec) where

import System.Taffybar.Widget.NetworkGraph
import Test.Hspec

spec :: Spec
spec = describe "Network graph tooltip" $ do
  it "reports only the interfaces included in the graph" $ do
    let config =
          defaultNetworkGraphConfig
            { interfacesFilter = (== "eth0"),
              networkGraphTooltipFormat = Just ("$inB$/$outB$", 2)
            }
        samples = [("eth0", (1024, 2048)), ("lo", (10000, 20000))]
    networkGraphTooltip config samples `shouldBe` Just "2048.0/1024.0"

  it "omits disabled tooltips" $
    networkGraphTooltip defaultNetworkGraphConfig {networkGraphTooltipFormat = Nothing} []
      `shouldBe` Nothing
