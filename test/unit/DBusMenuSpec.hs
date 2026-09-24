{-# LANGUAGE OverloadedStrings #-}

module DBusMenuSpec (spec) where

import DBus (toVariant)
import DBusMenu
  ( LayoutNode (..),
    MenuItemShape,
    menuItemLabel,
    menuItemShape,
  )
import DBusMenu.Reconcile
  ( ReconcileAction (..),
    planLabeledReconciliation,
  )
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Test.Hspec

spec :: Spec
spec = do
  describe "DBusMenu reconciliation" $ do
    it "reuses IDs whose GTK shape is unchanged" $ do
      let original = leaf 1 "Before" True
          updated = leaf 1 "After" False
      planLabeledReconciliation (existing [original]) (desired [updated])
        `shouldBe` [ReuseItem 1]

    it "reuses stable IDs across additions, removals, and reordering" $ do
      let old = [leaf 1 "A" True, leaf 2 "B" True, leaf 3 "C" True]
          new = [leaf 3 "C" True, leaf 2 "B" True, leaf 4 "D" True]
      planLabeledReconciliation (existing old) (desired new)
        `shouldBe` [ReuseItem 3, ReuseItem 2, BuildItem 4]

    it "reuses items by shape and label when the service renumbers everything" $ do
      let old = [leaf 1 "Wi-Fi" True, separator 2, submenu 3 "VPN", leaf 4 "Quit" True]
          new = [leaf 41 "Wi-Fi" False, separator 42, submenu 43 "VPN", leaf 44 "Quit" True]
      planLabeledReconciliation (existing old) (desired new)
        `shouldBe` [ReuseItem 1, ReuseItem 2, ReuseItem 3, ReuseItem 4]

    it "prefers exact ID matches over label matches" $ do
      let old = [leaf 1 "Same" True, leaf 2 "Same" True]
          new = [leaf 9 "Same" True, leaf 1 "Same" True]
      planLabeledReconciliation (existing old) (desired new)
        `shouldBe` [ReuseItem 2, ReuseItem 1]

    it "builds a replacement when an item's GTK shape changes" $ do
      let original = leaf 1 "Leaf" True
          updated = submenu 1 "Leaf"
      planLabeledReconciliation (existing [original]) (desired [updated])
        `shouldBe` [BuildItem 1]

    it "does not reuse the same widget for a duplicate desired ID" $ do
      let item = leaf 1 "Leaf" True
      planLabeledReconciliation (existing [item]) (desired [item, item])
        `shouldBe` [ReuseItem 1, BuildItem 1]

existing :: [LayoutNode] -> Map.Map Int32 (MenuItemShape, String)
existing nodes =
  Map.fromList [(lnId node, (menuItemShape node, menuItemLabel node)) | node <- nodes]

desired :: [LayoutNode] -> [(Int32, MenuItemShape, String)]
desired nodes = [(lnId node, menuItemShape node, menuItemLabel node) | node <- nodes]

leaf :: Int -> String -> Bool -> LayoutNode
leaf itemId label enabled =
  LayoutNode
    { lnId = fromIntegral itemId,
      lnProps =
        Map.fromList
          [ ("label", toVariant label),
            ("enabled", toVariant enabled)
          ],
      lnChildren = []
    }

separator :: Int -> LayoutNode
separator itemId =
  LayoutNode
    { lnId = fromIntegral itemId,
      lnProps = Map.singleton "type" (toVariant ("separator" :: String)),
      lnChildren = []
    }

submenu :: Int -> String -> LayoutNode
submenu itemId label =
  LayoutNode
    { lnId = fromIntegral itemId,
      lnProps =
        Map.fromList
          [ ("label", toVariant label),
            ("children-display", toVariant ("submenu" :: String))
          ],
      lnChildren = []
    }
