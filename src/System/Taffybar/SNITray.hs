-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.SNITray
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------
module System.Taffybar.SNITray where

import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import qualified GI.Gtk
import qualified Graphics.UI.Gtk as Gtk
import qualified StatusNotifier.Host.Service as H
import           StatusNotifier.Tray
import           System.Posix.Process
import           System.Taffybar.Compat.GtkLibs
import           System.Taffybar.Context
import           Text.Printf

getHost :: TaffyIO H.Host
getHost = do
  getStateDefault build
  where build = do
            pid <- lift getProcessID
            client <- asks dbusClient
            Just host <- lift $ H.build H.defaultParams
               { H.dbusClient = Just client
               , H.uniqueIdentifier = printf "taffybar-%s" $ show pid
               }
            return host

buildSNITray :: TaffyIO Gtk.Widget
buildSNITray = do
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
        }
    GI.Gtk.toWidget tray >>= fromGIWidget
