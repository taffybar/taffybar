{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

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
module System.Taffybar.Widget.SNITray
  ( TrayPriorityConfig,
    module System.Taffybar.Widget.SNITray,
  )
where

import Control.Monad (void)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified StatusNotifier.Host.Service as H
import StatusNotifier.Tray hiding (TrayItemMatcher, TrayParams, defaultTrayPriorityConfig)
import qualified StatusNotifier.Tray as Tray
import System.Posix.Process
import System.Taffybar.Context
import System.Taffybar.Widget.Util
import Text.Printf

-- | Parameters controlling tray construction and orientation.
type TrayParams = Tray.TrayParams

-- | Predicate used to match tray items in priority configuration.
type TrayItemMatcher = Tray.TrayItemMatcher

-- | Default tray icon priority configuration.
defaultTrayPriorityConfig :: TrayPriorityConfig
defaultTrayPriorityConfig = Tray.defaultTrayPriorityConfig

-- | Configuration for the base SNI tray widget.
data SNITrayConfig = SNITrayConfig
  { sniTrayTrayParams :: TrayParams,
    sniTrayPriorityConfig :: TrayPriorityConfig
  }

-- | Default 'SNITrayConfig'.
defaultSNITrayConfig :: SNITrayConfig
defaultSNITrayConfig =
  SNITrayConfig
    { sniTrayTrayParams = defaultTrayParams,
      sniTrayPriorityConfig = defaultTrayPriorityConfig
    }

-- | Configuration for the collapsible SNI tray widget.
data CollapsibleSNITrayParams = CollapsibleSNITrayParams
  { -- | Base tray configuration used to build the underlying SNI tray.
    collapsibleSNITrayConfig :: SNITrayConfig,
    -- | Maximum number of tray icons to show when collapsed.
    --
    -- Non-positive values disable collapsing.
    collapsibleSNITrayMaxVisibleIcons :: Int,
    -- | Whether the tray starts expanded.
    collapsibleSNITrayStartExpanded :: Bool,
    -- | Show the indicator while expanded.
    -- Useful to allow clicking it again to collapse.
    collapsibleSNITrayShowIndicatorWhenExpanded :: Bool,
    -- | Label renderer for the indicator.
    --
    -- Arguments: @(hiddenCount, expanded)@.
    collapsibleSNITrayIndicatorLabel :: Int -> Bool -> T.Text
  }

-- | Default indicator label renderer for the collapsible tray.
defaultCollapsibleSNITrayIndicatorLabel :: Int -> Bool -> T.Text
defaultCollapsibleSNITrayIndicatorLabel hiddenCount expanded
  | expanded = "-"
  | otherwise = "+" <> T.pack (show hiddenCount)

-- | Default 'CollapsibleSNITrayParams'.
defaultCollapsibleSNITrayParams :: CollapsibleSNITrayParams
defaultCollapsibleSNITrayParams =
  CollapsibleSNITrayParams
    { collapsibleSNITrayConfig = defaultSNITrayConfig,
      collapsibleSNITrayMaxVisibleIcons = 6,
      collapsibleSNITrayStartExpanded = False,
      collapsibleSNITrayShowIndicatorWhenExpanded = True,
      collapsibleSNITrayIndicatorLabel = defaultCollapsibleSNITrayIndicatorLabel
    }

-- | Build a new StatusNotifierItem tray that will share a host with any other
-- trays that are constructed automatically
sniTrayNew :: TaffyIO Gtk.Widget
sniTrayNew = sniTrayNewFromConfig defaultSNITrayConfig

-- | Build a new StatusNotifierItem tray from custom 'SNITrayConfig'.
sniTrayNewFromConfig :: SNITrayConfig -> TaffyIO Gtk.Widget
sniTrayNewFromConfig config =
  getTrayHost False >>= sniTrayNewFromHostConfig config

-- | Build a new StatusNotifierItem tray from the provided 'TrayParams'.
sniTrayNewFromParams :: TrayParams -> TaffyIO Gtk.Widget
sniTrayNewFromParams params =
  sniTrayNewFromConfig $
    defaultSNITrayConfig {sniTrayTrayParams = params}

-- | Build a new StatusNotifierItem tray from the provided 'TrayParams' and
-- 'H.Host'.
sniTrayNewFromHostParams :: TrayParams -> H.Host -> TaffyIO Gtk.Widget
sniTrayNewFromHostParams params =
  sniTrayNewFromHostConfig $
    defaultSNITrayConfig {sniTrayTrayParams = params}

-- | Build a new StatusNotifierItem tray from custom 'SNITrayConfig' and
-- 'H.Host'.
sniTrayNewFromHostConfig :: SNITrayConfig -> H.Host -> TaffyIO Gtk.Widget
sniTrayNewFromHostConfig SNITrayConfig {..} host = do
  client <- asks sessionDBusClient
  lift $ do
    tray <-
      buildTrayWithPriority
        host
        client
        sniTrayTrayParams
        sniTrayPriorityConfig
    _ <- widgetSetClassGI tray "sni-tray"
    Gtk.widgetShowAll tray
    Gtk.toWidget tray

-- | Build a collapsible StatusNotifierItem tray with default params.
sniTrayCollapsibleNew :: TaffyIO Gtk.Widget
sniTrayCollapsibleNew =
  sniTrayCollapsibleNewFromParams defaultCollapsibleSNITrayParams

-- | Build a collapsible StatusNotifierItem tray from custom params.
sniTrayCollapsibleNewFromParams ::
  CollapsibleSNITrayParams -> TaffyIO Gtk.Widget
sniTrayCollapsibleNewFromParams params =
  getTrayHost False >>= sniTrayCollapsibleNewFromHostParams params

-- | Build a collapsible StatusNotifierItem tray from custom params and a host.
sniTrayCollapsibleNewFromHostParams ::
  CollapsibleSNITrayParams -> H.Host -> TaffyIO Gtk.Widget
sniTrayCollapsibleNewFromHostParams CollapsibleSNITrayParams {..} host = do
  client <- asks sessionDBusClient
  lift $ do
    let SNITrayConfig {..} = collapsibleSNITrayConfig
    tray <-
      buildTrayWithPriority
        host
        client
        sniTrayTrayParams
        sniTrayPriorityConfig
    _ <- widgetSetClassGI tray "sni-tray"
    outer <- Gtk.boxNew (trayOrientation sniTrayTrayParams) 0
    _ <- widgetSetClassGI outer "sni-tray-collapsible"

    indicatorLabel <- Gtk.labelNew Nothing
    _ <- widgetSetClassGI indicatorLabel "sni-tray-overflow-indicator-label"
    indicator <- Gtk.eventBoxNew
    _ <- widgetSetClassGI indicator "sni-tray-overflow-indicator"
    Gtk.containerAdd indicator indicatorLabel

    Gtk.boxPackStart outer tray False False 0
    Gtk.boxPackStart outer indicator False False 0

    expandedRef <- newIORef collapsibleSNITrayStartExpanded

    let maxVisible = collapsibleSNITrayMaxVisibleIcons
        refresh = do
          children <- Gtk.containerGetChildren tray

          expanded <- readIORef expandedRef
          let shouldLimit = maxVisible > 0
              visibleCount
                | shouldLimit && not expanded = maxVisible
                | otherwise = length children
              visibleChildren = take visibleCount children
              hiddenChildren = drop visibleCount children
              hiddenCount = length hiddenChildren
              showIndicator
                | expanded =
                    shouldLimit
                      && length children > maxVisible
                      && collapsibleSNITrayShowIndicatorWhenExpanded
                | otherwise = hiddenCount > 0
              indicatorText =
                collapsibleSNITrayIndicatorLabel hiddenCount expanded

          mapM_ Gtk.widgetShow visibleChildren
          mapM_ Gtk.widgetHide hiddenChildren

          if showIndicator
            then do
              Gtk.labelSetText indicatorLabel indicatorText
              Gtk.widgetSetTooltipText
                indicator
                ( Just $
                    if expanded
                      then "Collapse tray icons"
                      else T.pack $ "Show " ++ show hiddenCount ++ " more tray icons"
                )
              Gtk.widgetShowAll indicator
            else do
              Gtk.widgetSetTooltipText indicator Nothing
              Gtk.widgetHide indicator

          if expanded
            then addClassIfMissing "sni-tray-collapsible-expanded" outer
            else removeClassIfPresent "sni-tray-collapsible-expanded" outer

          return hiddenCount

    _ <- Gtk.onWidgetButtonPressEvent indicator $ \event -> do
      eventType <- Gdk.getEventButtonType event
      button <- Gdk.getEventButtonButton event
      if eventType == Gdk.EventTypeButtonPress && button == 1
        then do
          hiddenCount <- refresh
          expanded <- readIORef expandedRef
          if hiddenCount > 0 || expanded
            then modifyIORef' expandedRef not >> void refresh >> return True
            else return False
        else return False

    let queueRefresh _ _ =
          void $
            Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT_IDLE $
              void refresh >> return False
    handlerId <- H.addUpdateHandler host queueRefresh
    _ <- Gtk.onWidgetDestroy outer $ H.removeUpdateHandler host handlerId

    _ <- refresh
    Gtk.widgetShowAll outer
    Gtk.toWidget outer

-- | Build a new StatusNotifierItem tray that also starts its own watcher,
-- without depending on status-notifier-icon. This will not register applets
-- started before the watcher is started.
sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt :: TaffyIO Gtk.Widget
sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt =
  sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoItFromConfig
    defaultSNITrayConfig

-- | Build a new StatusNotifierItem tray that also starts its own watcher,
-- with custom 'SNITrayConfig'.
sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoItFromConfig ::
  SNITrayConfig -> TaffyIO Gtk.Widget
sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoItFromConfig config =
  getTrayHost True >>= sniTrayNewFromHostConfig config

-- | Build a collapsible StatusNotifierItem tray that also starts its own
-- watcher, without depending on status-notifier-icon.
sniTrayCollapsibleThatStartsWatcherEvenThoughThisIsABadWayToDoIt ::
  TaffyIO Gtk.Widget
sniTrayCollapsibleThatStartsWatcherEvenThoughThisIsABadWayToDoIt =
  getTrayHost True
    >>= sniTrayCollapsibleNewFromHostParams defaultCollapsibleSNITrayParams

-- | Get a 'H.Host' from 'TaffyIO' internal state, that can be used to construct
-- SNI tray widgets. The boolean parameter determines whether or not a watcher
-- will be started the first time 'getTrayHost' is invoked.
getTrayHost :: Bool -> TaffyIO H.Host
getTrayHost startWatcher = getStateDefault $ do
  pid <- lift getProcessID
  client <- asks sessionDBusClient
  Just host <-
    lift $
      H.build
        H.defaultParams
          { H.dbusClient = Just client,
            H.uniqueIdentifier = printf "taffybar-%s" $ show pid,
            H.startWatcher = startWatcher
          }
  return host
