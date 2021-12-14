{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.SNITray
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module exports functions for the construction of
-- StatusNotifierItem/AppIndicator tray widgets, supplied by the
-- "StatusNotifier.Tray" module from the gtk-sni-tray library. These widgets do
-- not support the older XEMBED protocol, although bridges like
-- xembed-sni-proxy do allow sni trays to provide limited support for XEMBED
-- tray icons.
--
-- Unless 'sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt' is used it is
-- necessary to run status-notifier-watcher from the
-- [status-notifier-item](https://github.com/taffybar/status-notifier-item)
-- package before starting taffybar when using the functions defined in this
-- module. Using 'sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt' is
-- generally not recommended, because it can lead to issues with the
-- registration of tray icons if taffybar crashes/restarts, or if tray icon
-- providing applications are ever started before taffybar.
-----------------------------------------------------------------------------

module System.Taffybar.Widget.SNITray
  ( TrayParams
  , module System.Taffybar.Widget.SNITray
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified GI.Gtk
import qualified StatusNotifier.Host.Service as H
import           StatusNotifier.Tray
import           System.Posix.Process
import           System.Taffybar.Context
import           System.Taffybar.Widget.Util
import           Text.Printf

-- | Build a new StatusNotifierItem tray that will share a host with any other
-- trays that are constructed automatically
sniTrayNew :: TaffyIO GI.Gtk.Widget
sniTrayNew = sniTrayNewFromParams defaultTrayParams

-- | Build a new StatusNotifierItem tray from the provided 'TrayParams'.
sniTrayNewFromParams :: TrayParams -> TaffyIO GI.Gtk.Widget
sniTrayNewFromParams params =
  getTrayHost False >>= sniTrayNewFromHostParams params

-- | Build a new StatusNotifierItem tray from the provided 'TrayParams' and
-- 'H.Host'.
sniTrayNewFromHostParams :: TrayParams -> H.Host -> TaffyIO GI.Gtk.Widget
sniTrayNewFromHostParams params host = do
  client <- asks sessionDBusClient
  lift $ do
    tray <- buildTray host client params
    _ <- widgetSetClassGI tray "sni-tray"
    GI.Gtk.widgetShowAll tray
    GI.Gtk.toWidget tray

-- | Build a new StatusNotifierItem tray that also starts its own watcher,
-- without depending on status-notifier-icon. This will not register applets
-- started before the watcher is started.
sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt :: TaffyIO GI.Gtk.Widget
sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt =
  getTrayHost True >>= sniTrayNewFromHostParams defaultTrayParams

-- | Get a 'H.Host' from 'TaffyIO' internal state, that can be used to construct
-- SNI tray widgets. The boolean parameter determines whether or not a watcher
-- will be started the first time 'getTrayHost' is invoked.
getTrayHost :: Bool -> TaffyIO H.Host
getTrayHost startWatcher = getStateDefault $ do
  pid <- lift getProcessID
  client <- asks sessionDBusClient
  Just host <- lift $ H.build H.defaultParams
     { H.dbusClient = Just client
     , H.uniqueIdentifier = printf "taffybar-%s" $ show pid
     , H.startWatcher = startWatcher
     }
  return host

