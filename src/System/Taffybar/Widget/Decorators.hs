{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.Widget.Decorators where

import           Control.Monad.IO.Class
import qualified GI.Gtk as Gtk
import           System.Taffybar.Widget.Util

-- | Wrap a widget with two container boxes. The inner box will have the class
-- "InnerPad", and the outer box will have the class "OuterPad". These boxes can
-- be used to add padding between the outline of the widget and its contents, or
-- for the purpose of displaying a different background behind the widget.
buildPadBox :: MonadIO m => Gtk.Widget -> m Gtk.Widget
buildPadBox contents = liftIO $ do
  innerBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
  outerBox <- Gtk.eventBoxNew
  Gtk.containerAdd innerBox contents
  Gtk.containerAdd outerBox innerBox
  _ <- widgetSetClassGI innerBox "inner-pad"
  _ <- widgetSetClassGI outerBox "outer-pad"
  Gtk.widgetShow outerBox
  Gtk.widgetShow innerBox
  Gtk.toWidget outerBox

buildContentsBox :: MonadIO m => Gtk.Widget -> m Gtk.Widget
buildContentsBox widget = liftIO $ do
  contents <- Gtk.boxNew Gtk.OrientationHorizontal 0
  Gtk.containerAdd contents widget
  _ <- widgetSetClassGI contents "contents"
  Gtk.widgetShowAll contents
  Gtk.toWidget contents >>= buildPadBox
