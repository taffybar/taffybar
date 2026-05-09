module System.Taffybar.Widget.SNITray.PrioritizedCollapsibleSpec (spec) where

import DBus (busName_, formatBusName, objectPath_)
import Data.Map.Strict qualified as M
import StatusNotifier.Host.Service qualified as H
import System.Taffybar.Widget.SNITray.PrioritizedCollapsible
import Test.Hspec

spec :: Spec
spec =
  describe "sortedInfosByPriority" $ do
    it "uses stable metadata before D-Bus address for equal priorities" $ do
      let beta =
            testItem
              ":1.1"
              "/StatusNotifierItem"
              (Just "statusnotifieritem-1")
              "Zeta"
              "zeta-icon"
          alpha =
            testItem
              ":1.9"
              "/StatusNotifierItem"
              (Just "statusnotifieritem-9")
              "Alpha"
              "alpha-icon"
          sorted =
            sortedInfosByPriority
              True
              (-5)
              5
              0
              M.empty
              (const Nothing)
              [beta, alpha]

      map H.iconTitle sorted `shouldBe` ["Alpha", "Zeta"]

    it "uses process keys before D-Bus address" $ do
      let zeta =
            testItem
              ":1.1"
              "/StatusNotifierItem"
              (Just "systray_1")
              "Zeta"
              "same-icon"
          alpha =
            testItem
              ":1.9"
              "/StatusNotifierItem"
              (Just "systray_9")
              "Alpha"
              "same-icon"
          processKey info =
            case formatBusName (H.itemServiceName info) of
              ":1.1" -> Just "process:zeta"
              ":1.9" -> Just "process:alpha"
              _ -> Nothing
          sorted =
            sortedInfosByPriority
              True
              (-5)
              5
              0
              M.empty
              processKey
              [zeta, alpha]

      map H.iconTitle sorted `shouldBe` ["Alpha", "Zeta"]

    it "keeps priority ahead of the semantic tie-breaker" $ do
      let beta =
            testItem
              ":1.1"
              "/StatusNotifierItem"
              Nothing
              "Beta"
              "beta-icon"
          alpha =
            testItem
              ":1.9"
              "/StatusNotifierItem"
              Nothing
              "Alpha"
              "alpha-icon"
          sorted =
            sortedInfosByPriority
              True
              (-5)
              5
              0
              (M.fromList [("icon-title:Beta", 1)])
              (const Nothing)
              [alpha, beta]

      map H.iconTitle sorted `shouldBe` ["Beta", "Alpha"]

testItem :: String -> String -> Maybe String -> String -> String -> H.ItemInfo
testItem serviceName servicePath itemId iconTitle iconName =
  H.ItemInfo
    { H.itemServiceName = busName_ serviceName,
      H.itemServicePath = objectPath_ servicePath,
      H.itemId = itemId,
      H.itemStatus = Nothing,
      H.itemCategory = Nothing,
      H.itemToolTip = Nothing,
      H.iconTitle = iconTitle,
      H.iconName = iconName,
      H.overlayIconName = Nothing,
      H.iconThemePath = Nothing,
      H.iconPixmaps = [],
      H.overlayIconPixmaps = [],
      H.menuPath = Nothing,
      H.itemIsMenu = True
    }
