module System.Taffybar.Widget.Decorators where

import           Control.Monad.IO.Class
import qualified Graphics.UI.Gtk as Gtk
import           System.Taffybar.Widget.Util

-- | Wrap a widget with two container boxes. The inner box will have the class
-- "InnerPad", and the outer box will have the class "OuterPad". These boxes can
-- be used to add padding between the outline of the widget and its contents, or
-- for the purpose of displaying a different background behind the widget.
buildPadBox :: (Gtk.WidgetClass widget, MonadIO m) => widget -> m Gtk.Widget
buildPadBox contents = liftIO $ do
  innerBox <- Gtk.hBoxNew False 0
  outerBox <- Gtk.eventBoxNew
  Gtk.containerAdd innerBox contents
  Gtk.containerAdd outerBox innerBox
  _ <- widgetSetClass innerBox "InnerPad"
  _ <- widgetSetClass outerBox "OuterPad"
  Gtk.widgetShowAll outerBox
  return $ Gtk.toWidget outerBox
