-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.SNITray
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------
module System.Taffybar.Widget.SNITray where

import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import qualified GI.Gtk
import           Graphics.UI.GIGtkStrut
import qualified Graphics.UI.Gtk as Gtk
import qualified StatusNotifier.Host.Service as H
import           StatusNotifier.Tray
import           System.Posix.Process
import           System.Taffybar.Compat.GtkLibs
import           System.Taffybar.Context
import           Text.Printf

getHost :: TaffyIO H.Host
getHost = getStateDefault $ do
  pid <- lift getProcessID
  client <- asks dbusClient
  Just host <- lift $ H.build H.defaultParams
     { H.dbusClient = Just client
     , H.uniqueIdentifier = printf "taffybar-%s" $ show pid
     }
  return host

-- | Build a new StatusNotifierItem tray that will share a host with any other
-- trays that are constructed automatically
sniTrayNew :: TaffyIO Gtk.Widget
sniTrayNew = do
  host <- getHost
  client <- asks dbusClient
  lift $ do
    tray <-
      buildTray
        TrayParams
        { trayHost = host
        , trayClient = client
        , trayOrientation = GI.Gtk.OrientationHorizontal
        , trayImageSize = Expand
        , trayIconExpand = False
        , trayAlignment = End
        }
    GI.Gtk.widgetShowAll tray
    GI.Gtk.toWidget tray >>= fromGIWidget
