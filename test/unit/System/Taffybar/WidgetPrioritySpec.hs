{-# LANGUAGE OverloadedStrings #-}

module System.Taffybar.WidgetPrioritySpec (spec) where

import GI.Gtk qualified as Gtk
import System.Taffybar.WidgetPriority
import Test.Hspec

-- GtkAdjustment is a plain GObject with no display dependency, so the
-- annotation round-trip can be tested without initializing GTK. The bar
-- applies the same mechanism to widgets.
newTestObject :: IO Gtk.Adjustment
newTestObject = Gtk.adjustmentNew 0 0 100 1 10 10

spec :: Spec
spec =
  describe "widget priority annotations" $ do
    it "round-trips priorities, including zero and negative values" $ do
      object <- newTestObject
      getWidgetPriority object `shouldReturn` Nothing
      setWidgetPriority 5 object
      getWidgetPriority object `shouldReturn` Just 5
      setWidgetPriority 0 object
      getWidgetPriority object `shouldReturn` Just 0
      setWidgetPriority (-3) object
      getWidgetPriority object `shouldReturn` Just (-3)

    it "keeps annotations on separate objects independent" $ do
      annotated <- newTestObject
      plain <- newTestObject
      setWidgetPriority 7 annotated
      getWidgetPriority annotated `shouldReturn` Just 7
      getWidgetPriority plain `shouldReturn` Nothing
