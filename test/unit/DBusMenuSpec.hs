{-# LANGUAGE OverloadedStrings #-}

module DBusMenuSpec (spec) where

import DBus (toVariant)
import DBusMenu
  ( LayoutNode (..),
    MenuItemShape,
    menuItemShape,
  )
import DBusMenu.Reconcile
  ( ReconcileAction (..),
    planReconciliation,
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
          existing :: Map.Map Int32 MenuItemShape
          existing = Map.singleton 1 (menuItemShape original)
      planReconciliation existing [(1, menuItemShape updated)]
        `shouldBe` [ReuseItem 1]

    it "reuses stable IDs across additions, removals, and reordering" $ do
      let shape = menuItemShape (leaf 0 "" True)
          existing :: Map.Map Int32 MenuItemShape
          existing = Map.fromList [(1, shape), (2, shape), (3, shape)]
      planReconciliation existing [(3, shape), (2, shape), (4, shape)]
        `shouldBe` [ReuseItem 3, ReuseItem 2, BuildItem 4]

    it "builds a replacement when an item's GTK shape changes" $ do
      let original = leaf 1 "Leaf" True
          updated = submenu 1 "Submenu"
          existing :: Map.Map Int32 MenuItemShape
          existing = Map.singleton 1 (menuItemShape original)
      planReconciliation existing [(1, menuItemShape updated)]
        `shouldBe` [BuildItem 1]

    it "does not reuse the same widget for a duplicate desired ID" $ do
      let shape = menuItemShape (leaf 1 "Leaf" True)
          existing :: Map.Map Int32 MenuItemShape
          existing = Map.singleton 1 shape
      planReconciliation existing [(1, shape), (1, shape)]
        `shouldBe` [ReuseItem 1, BuildItem 1]

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
