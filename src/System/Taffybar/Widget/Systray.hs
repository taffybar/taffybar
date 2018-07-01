-- | This is a very basic system tray widget.  That said, it works
-- very well since it is based on eggtraymanager.
module System.Taffybar.Widget.Systray {-# DEPRECATED "Use SNITray instead" #-} ( systrayNew ) where

import Control.Monad.IO.Class
import qualified GI.Gtk
import System.Taffybar.Compat.GtkLibs
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Misc.TrayManager

systrayNew :: MonadIO m => m GI.Gtk.Widget
systrayNew = liftIO $ do
  box <- hBoxNew False 5

  trayManager <- trayManagerNew
  Just screen <- screenGetDefault
  _ <- trayManagerManageScreen trayManager screen

  _ <- on trayManager trayIconAdded $ \w -> do
    widgetShowAll w
    boxPackStart box w PackNatural 0

  widgetShowAll box
  toGIWidget (toWidget box)
