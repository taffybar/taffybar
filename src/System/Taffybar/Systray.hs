-- | This is a very basic system tray widget.  That said, it works
-- very well since it is based on eggtraymanager.
module System.Taffybar.Systray ( systrayNew ) where

import Control.Monad.Trans
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Misc.TrayManager

systrayNew :: MonadIO m => m Widget
systrayNew = liftIO $ do
  box <- hBoxNew False 5

  trayManager <- trayManagerNew
  Just screen <- screenGetDefault
  _ <- trayManagerManageScreen trayManager screen

  _ <- on trayManager trayIconAdded $ \w -> do
    widgetShowAll w
    boxPackStart box w PackNatural 0

  widgetShowAll box
  return (toWidget box)
