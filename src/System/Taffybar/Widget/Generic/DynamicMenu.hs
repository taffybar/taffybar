module System.Taffybar.Widget.Generic.DynamicMenu where

import           Control.Monad.IO.Class
import qualified GI.Gtk as Gtk

data DynamicMenuConfig = DynamicMenuConfig
  { dmClickWidget :: Gtk.Widget
  , dmPopulateMenu :: Gtk.Menu -> IO ()
  }

dynamicMenuNew :: MonadIO m => DynamicMenuConfig -> m Gtk.Widget
dynamicMenuNew DynamicMenuConfig
                 { dmClickWidget = clickWidget
                 , dmPopulateMenu = populateMenu
                 } = do
  button <- Gtk.menuButtonNew
  menu <- Gtk.menuNew
  Gtk.containerAdd button clickWidget
  Gtk.menuButtonSetPopup button $ Just menu

  _ <- Gtk.onButtonPressed button $ emptyMenu menu >> populateMenu menu

  Gtk.widgetShowAll button

  Gtk.toWidget button

emptyMenu :: (Gtk.IsContainer a, MonadIO m) => a -> m ()
emptyMenu menu =
  Gtk.containerForeach menu $ \item ->
    Gtk.containerRemove menu item >> Gtk.widgetDestroy item
