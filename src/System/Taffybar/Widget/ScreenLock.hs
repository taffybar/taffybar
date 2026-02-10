{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.ScreenLock
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides a screen-lock widget that displays a lock icon and
-- provides quick access to locking the screen and toggling idle inhibition.
--
-- The widget responds to mouse clicks:
--
--   * __Left-click__: Opens a popup menu with "Lock Screen" and an "Idle
--     Inhibitor" toggle.
--   * __Right-click__: Instantly locks the screen by spawning hyprlock.
--
-- The widget applies CSS class @screen-lock-inhibited@ when the idle
-- inhibitor is active, allowing visual differentiation via stylesheets.
--
-- Example usage:
--
-- > import System.Taffybar.Widget.ScreenLock
-- >
-- > -- Simple usage with defaults
-- > let lockWidget = screenLockNew
-- >
-- > -- Custom icon and inhibit types
-- > let custom = screenLockNewWithConfig defaultScreenLockConfig
-- >       { screenLockIcon = "\xF023"
-- >       , screenLockInhibitTypes = [InhibitIdle, InhibitSleep]
-- >       }
-----------------------------------------------------------------------------
module System.Taffybar.Widget.ScreenLock
  ( screenLockNew
  , screenLockNewWithConfig
  , ScreenLockConfig(..)
  , defaultScreenLockConfig
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Default (Default(..))
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.Information.Inhibitor (InhibitType(..), InhibitorState(..))
import           System.Taffybar.Information.ScreenLock
import           System.Taffybar.Util (postGUIASync)
import           System.Taffybar.Widget.Generic.ChannelWidget
import           System.Taffybar.Widget.Util (widgetSetClassGI, addClassIfMissing, removeClassIfPresent)

-- | Configuration for the screen lock widget.
data ScreenLockConfig = ScreenLockConfig
  { -- | Icon text to display (default: U+F023, nf-fa-lock).
    screenLockIcon :: T.Text
    -- | What types of inhibitors to manage (default: @[InhibitIdle]@).
  , screenLockInhibitTypes :: [InhibitType]
  } deriving (Eq, Show)

-- | Default configuration for the screen lock widget.
defaultScreenLockConfig :: ScreenLockConfig
defaultScreenLockConfig = ScreenLockConfig
  { screenLockIcon = T.pack "\xF023"
  , screenLockInhibitTypes = [InhibitIdle]
  }

instance Default ScreenLockConfig where
  def = defaultScreenLockConfig

screenLockLogPath :: String
screenLockLogPath = "System.Taffybar.Widget.ScreenLock"

screenLockLog :: MonadIO m => Priority -> String -> m ()
screenLockLog priority = liftIO . logM screenLockLogPath priority

-- | Create a screen lock widget with default configuration.
screenLockNew :: TaffyIO Gtk.Widget
screenLockNew = screenLockNewWithConfig defaultScreenLockConfig

-- | Create a screen lock widget with custom configuration.
--
-- The widget displays a lock icon inside an event box. Left-clicking opens
-- a popup menu with a "Lock Screen" action and an "Idle Inhibitor" toggle.
-- Right-clicking immediately locks the screen.
--
-- CSS class @screen-lock@ is always applied to the event box. When the idle
-- inhibitor is active, @screen-lock-inhibited@ is added.
screenLockNewWithConfig :: ScreenLockConfig -> TaffyIO Gtk.Widget
screenLockNewWithConfig config = do
  let types = screenLockInhibitTypes config
  chan <- getInhibitorChan types
  ctx <- ask

  liftIO $ do
    label <- Gtk.labelNew Nothing
    Gtk.labelSetText label (screenLockIcon config)

    ebox <- Gtk.eventBoxNew
    Gtk.containerAdd ebox label
    _ <- widgetSetClassGI ebox "screen-lock"

    let updateWidget state = postGUIASync $ do
          let active = inhibitorActive state
          if active
            then addClassIfMissing "screen-lock-inhibited" ebox
            else removeClassIfPresent "screen-lock-inhibited" ebox
          let tooltipText =
                if active
                then "Screen Lock (idle inhibitor active)"
                else "Screen Lock"
          Gtk.widgetSetTooltipText ebox (Just tooltipText)

    -- Set initial state on realize
    void $ Gtk.onWidgetRealize ebox $ do
      initialState <- runReaderT (getInhibitorState types) ctx
      updateWidget initialState

    -- Click handler
    void $ Gtk.onWidgetButtonPressEvent ebox $ \event -> do
      eventType <- Gdk.getEventButtonType event
      button <- Gdk.getEventButtonButton event
      if eventType /= Gdk.EventTypeButtonPress
        then return False
        else case button of
          1 -> do
            showScreenLockMenu ctx config ebox
            return True
          3 -> do
            screenLockLog DEBUG "Right-click: locking screen"
            lockScreen
            return True
          _ -> return False

    Gtk.widgetShowAll ebox
    Gtk.toWidget =<< channelWidgetNew ebox chan updateWidget

-- | Build and show the popup menu for the screen lock widget.
showScreenLockMenu :: Context -> ScreenLockConfig -> Gtk.EventBox -> IO ()
showScreenLockMenu ctx config ebox = do
  let types = screenLockInhibitTypes config
  currentEvent <- Gtk.getCurrentEvent

  menu <- Gtk.menuNew
  Gtk.menuAttachToWidget menu ebox Nothing

  -- "Lock Screen" item
  lockItem <- Gtk.menuItemNewWithLabel ("Lock Screen" :: T.Text)
  void $ Gtk.onMenuItemActivate lockItem $ do
    screenLockLog DEBUG "Menu: locking screen"
    lockScreen
  Gtk.menuShellAppend menu lockItem

  -- Separator
  sep <- Gtk.separatorMenuItemNew
  Gtk.menuShellAppend menu sep

  -- "Idle Inhibitor" check menu item
  inhibitItem <- Gtk.checkMenuItemNewWithLabel ("Idle Inhibitor" :: T.Text)
  currentState <- runReaderT (getInhibitorState types) ctx
  Gtk.checkMenuItemSetActive inhibitItem (inhibitorActive currentState)
  void $ Gtk.onCheckMenuItemToggled inhibitItem $ do
    screenLockLog DEBUG "Menu: toggling idle inhibitor"
    runReaderT (toggleInhibitor types) ctx
  Gtk.menuShellAppend menu inhibitItem

  -- Destroy menu when hidden (same pattern as SNIMenu)
  void $ Gtk.onWidgetHide menu $
    void $ GLib.idleAdd GLib.PRIORITY_LOW $ do
      Gtk.widgetDestroy menu
      return False

  Gtk.widgetShowAll menu
  Gtk.menuPopupAtPointer menu currentEvent
