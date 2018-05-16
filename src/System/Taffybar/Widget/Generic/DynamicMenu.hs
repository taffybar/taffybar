module System.Taffybar.Widget.Generic.DynamicMenu where

import           Control.Monad.IO.Class
import qualified GI.Gtk as Gtk
import           System.Taffybar.Widget.Util

data DynamicMenuConfig = DynamicMenuConfig
  { dmClickWidget :: Gtk.Widget
  , dmPopulateMenu :: Gtk.Menu -> IO ()
  }

dynamicMenuNew :: MonadIO m => DynamicMenuConfig -> m Gtk.Widget
dynamicMenuNew DynamicMenuConfig { dmClickWidget = clickWidget
                                 , dmPopulateMenu = populateMenu
                                 } = do
  bar <- Gtk.menuBarNew
  menu <- Gtk.menuNew
  menuItem <- Gtk.menuItemNew
  Gtk.containerAdd menuItem clickWidget
  Gtk.menuItemSetSubmenu menuItem $ Just menu
  Gtk.containerAdd bar menuItem
  _ <- widgetSetClassGI menu "Menu"

  _ <- Gtk.onMenuItemActivate menuItem $ populateMenu menu
  _ <- Gtk.onMenuItemDeselect menuItem $ emptyMenu menu

  Gtk.widgetShowAll bar

  Gtk.toWidget bar

emptyMenu :: (Gtk.IsContainer a, MonadIO m) => a -> m ()
emptyMenu menu =
  Gtk.containerForeach menu $ \item ->
    Gtk.containerRemove menu item >> Gtk.widgetDestroy item
