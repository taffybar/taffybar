-- | Build menu buttons whose menu contents are rebuilt each time they are
-- opened.
module System.Taffybar.Widget.Generic.DynamicMenu where

import Control.Monad.IO.Class
import qualified GI.Gtk as Gtk

-- | Configuration for a dynamic menu button.
data DynamicMenuConfig = DynamicMenuConfig
  { dmClickWidget :: Gtk.Widget,
    dmPopulateMenu :: Gtk.Menu -> IO ()
  }

-- | Wrap a widget in a menu button and repopulate the attached menu on click.
dynamicMenuNew :: (MonadIO m) => DynamicMenuConfig -> m Gtk.Widget
dynamicMenuNew
  DynamicMenuConfig
    { dmClickWidget = clickWidget,
      dmPopulateMenu = populateMenu
    } = do
    button <- Gtk.menuButtonNew
    menu <- Gtk.menuNew
    Gtk.containerAdd button clickWidget
    Gtk.menuButtonSetPopup button $ Just menu

    _ <- Gtk.onButtonPressed button $ emptyMenu menu >> populateMenu menu

    Gtk.widgetShowAll button

    Gtk.toWidget button

-- | Remove and destroy all current menu items.
emptyMenu :: (Gtk.IsContainer a, MonadIO m) => a -> m ()
emptyMenu menu =
  Gtk.containerForeach menu $ \item ->
    Gtk.containerRemove menu item >> Gtk.widgetDestroy item
