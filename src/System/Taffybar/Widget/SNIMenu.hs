{-# LANGUAGE OverloadedStrings #-}

module System.Taffybar.Widget.SNIMenu
  ( withSniMenu,
    withNmAppletMenu,
  )
where

import Control.Exception.Enclosed (catchAny)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import DBus (BusName, ObjectPath)
import qualified DBusMenu
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk (EventType (..), getEventButtonButton, getEventButtonType)
import qualified GI.Gtk as Gtk
import System.Log.Logger (Priority (..), logM)
import System.Taffybar.Context (Context (..), TaffyIO)
import Text.Printf (printf)

sniMenuLogger :: Priority -> String -> IO ()
sniMenuLogger = logM "System.Taffybar.Widget.SNIMenu"

-- | Wrap any widget so that clicking it pops up an SNI application's
-- DBusMenu.  The menu is built on each click via
-- 'StatusNotifier.DBusMenu.buildMenu' and destroyed when hidden.
withSniMenu :: BusName -> ObjectPath -> TaffyIO Gtk.Widget -> TaffyIO Gtk.Widget
withSniMenu busName menuPath mkWidget = do
  ctx <- ask
  inner <- mkWidget
  liftIO $ do
    ebox <- Gtk.eventBoxNew
    Gtk.containerAdd ebox inner

    _ <- Gtk.onWidgetButtonPressEvent ebox $ \event -> do
      eventType <- Gdk.getEventButtonType event
      button <- Gdk.getEventButtonButton event
      if eventType == Gdk.EventTypeButtonPress && button == 1
        then do
          currentEvent <- Gtk.getCurrentEvent
          catchAny
            ( do
                let client = sessionDBusClient ctx
                gtkMenu <- DBusMenu.buildMenu client busName menuPath
                Gtk.menuAttachToWidget gtkMenu ebox Nothing
                _ <- Gtk.onWidgetHide gtkMenu $
                  void $
                    GLib.idleAdd GLib.PRIORITY_LOW $ do
                      Gtk.widgetDestroy gtkMenu
                      return False
                Gtk.widgetShowAll gtkMenu
                Gtk.menuPopupAtPointer gtkMenu currentEvent
            )
            ( sniMenuLogger WARNING
                . printf "Failed to build SNI menu for %s: %s" (show busName)
                . show
            )
          return True
        else return False

    Gtk.widgetShowAll ebox
    Gtk.toWidget ebox

-- | Convenience wrapper: click to open nm-applet's network menu.
withNmAppletMenu :: TaffyIO Gtk.Widget -> TaffyIO Gtk.Widget
withNmAppletMenu =
  withSniMenu
    "org.freedesktop.network-manager-applet"
    "/org/ayatana/NotificationItem/nm_applet/Menu"
