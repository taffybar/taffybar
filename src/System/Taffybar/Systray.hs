-- | This is a very basic system tray widget.  That said, it works
-- very well since it is based on eggtraymanager.
module System.Taffybar.Systray ( systrayNew ) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Misc.TrayManager

systrayNew :: IO Widget
systrayNew = do
  box <- hBoxNew False 5
  widgetModifyBg box StateNormal (Color 0 0 0)

  trayManager <- trayManagerNew
  Just screen <- screenGetDefault
  _ <- trayManagerManageScreen trayManager screen

  _ <- on trayManager trayIconAdded $ \w -> do
    widgetModifyBg w StateNormal (Color 0 0 0)
    widgetShowAll w
    boxPackStart box w PackNatural 0

  _ <- on trayManager trayIconRemoved $ \w -> do
    putStrLn "Tray icon removed"

  widgetShowAll box
  return (toWidget box)