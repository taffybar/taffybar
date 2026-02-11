{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Wlsunset
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides a clickable widget for controlling @wlsunset@, a
-- Wayland day\/night gamma adjustor.
--
-- The widget displays a sun icon whose CSS class reflects the current
-- wlsunset state:
--
--   * @wlsunset-cool@ -- normal (forced day temperature, no shift)
--   * @wlsunset-warm@ -- forced warm (night) temperature
--   * @wlsunset-off@  -- process stopped
--   * (no extra class) -- automatic\/running normally
--
-- Left-clicking opens a popup menu for mode selection and start\/stop.
-- Right-clicking quick-toggles the process on\/off.
module System.Taffybar.Widget.Wlsunset
  ( wlsunsetNew,
    wlsunsetNewWithConfig,
    WlsunsetWidgetConfig (..),
    defaultWlsunsetWidgetConfig,
  )
where

import Control.Monad (forM_, replicateM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Default (Default (..))
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (Context, TaffyIO)
import System.Taffybar.Information.Wlsunset
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)

-- | Widget-level configuration for the wlsunset widget. Wraps the
-- underlying 'WlsunsetConfig' from the Information layer.
data WlsunsetWidgetConfig = WlsunsetWidgetConfig
  { -- | The underlying information-layer config.
    wlsunsetWidgetInfoConfig :: WlsunsetConfig,
    -- | Text icon to display (default: sun icon U+F0599, nf-md-white_balance_sunny).
    wlsunsetWidgetIcon :: T.Text
  }
  deriving (Eq, Show)

-- | Default widget configuration.
defaultWlsunsetWidgetConfig :: WlsunsetWidgetConfig
defaultWlsunsetWidgetConfig =
  WlsunsetWidgetConfig
    { wlsunsetWidgetInfoConfig = def,
      wlsunsetWidgetIcon = T.pack "\xF0599"
    }

instance Default WlsunsetWidgetConfig where
  def = defaultWlsunsetWidgetConfig

-- | All CSS classes that the widget may toggle based on state.
allStateClasses :: [T.Text]
allStateClasses = ["wlsunset-warm", "wlsunset-cool", "wlsunset-off"]

-- | Create a wlsunset widget with the default configuration.
wlsunsetNew :: TaffyIO Gtk.Widget
wlsunsetNew = wlsunsetNewWithConfig defaultWlsunsetWidgetConfig

-- | Create a wlsunset widget with a custom configuration.
--
-- The widget is a clickable event box containing a nerd-font sun icon.
-- CSS classes are updated reactively via a broadcast channel from the
-- Information layer. Left-click opens a popup menu; right-click toggles
-- the process.
wlsunsetNewWithConfig :: WlsunsetWidgetConfig -> TaffyIO Gtk.Widget
wlsunsetNewWithConfig widgetCfg = do
  let infoCfg = wlsunsetWidgetInfoConfig widgetCfg
  chan <- getWlsunsetChan infoCfg
  ctx <- ask

  liftIO $ do
    label <- Gtk.labelNew (Just (wlsunsetWidgetIcon widgetCfg))

    ebox <- Gtk.eventBoxNew
    Gtk.containerAdd ebox label

    styleCtx <- Gtk.widgetGetStyleContext ebox
    Gtk.styleContextAddClass styleCtx "wlsunset"

    let updateWidget st = postGUIASync $ do
          updateStateClasses ebox st
          updateTooltip ebox st

    void $ Gtk.onWidgetRealize ebox $ do
      initialState <- runReaderT (getWlsunsetState infoCfg) ctx
      updateWidget initialState

    setupClickHandler ctx ebox infoCfg
    Gtk.widgetShowAll ebox
    Gtk.toWidget =<< channelWidgetNew ebox chan updateWidget

-- ---------------------------------------------------------------------------
-- CSS class management
-- ---------------------------------------------------------------------------

-- | Update CSS classes on a widget based on the current wlsunset state.
updateStateClasses :: (Gtk.IsWidget w) => w -> WlsunsetState -> IO ()
updateStateClasses widget st = do
  styleCtx <- Gtk.widgetGetStyleContext widget
  -- Remove all state classes first
  mapM_ (Gtk.styleContextRemoveClass styleCtx) allStateClasses
  -- Add the appropriate class
  forM_ (stateToClass st) (Gtk.styleContextAddClass styleCtx)

-- | Map a 'WlsunsetState' to its CSS class, or 'Nothing' for auto/running.
stateToClass :: WlsunsetState -> Maybe T.Text
stateToClass st
  | not (wlsunsetRunning st) = Just "wlsunset-off"
  | otherwise = case wlsunsetMode st of
      WlsunsetAuto -> Nothing
      WlsunsetForcedWarm -> Just "wlsunset-warm"
      WlsunsetForcedCool -> Just "wlsunset-cool"

-- ---------------------------------------------------------------------------
-- Tooltip
-- ---------------------------------------------------------------------------

-- | Update the tooltip text to reflect the current state.
updateTooltip :: (Gtk.IsWidget w) => w -> WlsunsetState -> IO ()
updateTooltip widget st = do
  let text = case (wlsunsetRunning st, wlsunsetMode st) of
        (False, _) -> "wlsunset: stopped"
        (True, WlsunsetAuto) -> "wlsunset: automatic"
        (True, WlsunsetForcedCool) -> "wlsunset: normal (no shift)"
        (True, WlsunsetForcedWarm) -> "wlsunset: forced warm"
  Gtk.widgetSetTooltipText widget (Just text)

-- ---------------------------------------------------------------------------
-- Click handling
-- ---------------------------------------------------------------------------

-- | Set up button-press handlers on the event box.
-- Left-click (button 1) opens a popup menu.
-- Right-click (button 3) quick-toggles the process.
setupClickHandler :: Context -> Gtk.EventBox -> WlsunsetConfig -> IO ()
setupClickHandler ctx ebox infoCfg =
  void $ Gtk.onWidgetButtonPressEvent ebox $ \event -> do
    eventType <- Gdk.getEventButtonType event
    button <- Gdk.getEventButtonButton event
    if eventType /= Gdk.EventTypeButtonPress
      then return False
      else case button of
        1 -> do
          showPopupMenu ctx ebox infoCfg
          return True
        3 -> do
          runReaderT (toggleWlsunset infoCfg) ctx
          return True
        _ -> return False

-- ---------------------------------------------------------------------------
-- Popup menu
-- ---------------------------------------------------------------------------

-- | Build and show a popup menu attached to the event box.
showPopupMenu :: Context -> Gtk.EventBox -> WlsunsetConfig -> IO ()
showPopupMenu ctx ebox infoCfg = do
  currentEvent <- Gtk.getCurrentEvent
  st <- runReaderT (getWlsunsetState infoCfg) ctx

  menu <- Gtk.menuNew
  Gtk.menuAttachToWidget menu ebox Nothing

  -- Mode items (disabled when not running)
  let running = wlsunsetRunning st
      currentMode = wlsunsetMode st
      modes =
        [ ("Automatic", WlsunsetAuto),
          ("Normal", WlsunsetForcedCool),
          ("Force Warm", WlsunsetForcedWarm)
        ]

  forM_ modes $ \(labelText, targetMode) -> do
    let prefix =
          if running && currentMode == targetMode
            then "\x2713 " :: T.Text -- checkmark
            else "   "
    item <- Gtk.menuItemNewWithLabel (prefix <> labelText)
    Gtk.widgetSetSensitive item running
    void $
      Gtk.onMenuItemActivate item $
        runReaderT (cycleToMode infoCfg currentMode targetMode) ctx
    Gtk.menuShellAppend menu item

  -- Separator
  sep <- Gtk.separatorMenuItemNew
  Gtk.menuShellAppend menu sep

  -- Start/Stop item
  let toggleLabel =
        if running
          then "Stop wlsunset" :: T.Text
          else "Start wlsunset"
  toggleItem <- Gtk.menuItemNewWithLabel toggleLabel
  void $
    Gtk.onMenuItemActivate toggleItem $
      runReaderT (toggleWlsunset infoCfg) ctx
  Gtk.menuShellAppend menu toggleItem

  -- Cleanup: destroy menu after it hides
  void $
    Gtk.onWidgetHide menu $
      void $
        GLib.idleAdd GLib.PRIORITY_LOW $ do
          Gtk.widgetDestroy menu
          return False

  Gtk.widgetShowAll menu
  Gtk.menuPopupAtPointer menu currentEvent

-- ---------------------------------------------------------------------------
-- Mode cycling helper
-- ---------------------------------------------------------------------------

-- | Cycle from the current mode to a target mode. The mode ring is:
-- Auto -> ForcedWarm -> ForcedCool -> Auto. We calculate the number of
-- SIGUSR1 signals needed and send them.
cycleToMode :: WlsunsetConfig -> WlsunsetMode -> WlsunsetMode -> TaffyIO ()
cycleToMode infoCfg currentMode targetMode = do
  let cyclesNeeded = cyclesToReach currentMode targetMode
  replicateM_ cyclesNeeded (cycleWlsunsetMode infoCfg)

-- | Calculate how many SIGUSR1 cycles are needed to go from one mode
-- to another in the ring Auto -> ForcedCool -> ForcedWarm -> Auto.
cyclesToReach :: WlsunsetMode -> WlsunsetMode -> Int
cyclesToReach from to
  | from == to = 0
  | otherwise = (toOrd to - toOrd from) `mod` 3
  where
    toOrd :: WlsunsetMode -> Int
    toOrd WlsunsetAuto = 0
    toOrd WlsunsetForcedCool = 1
    toOrd WlsunsetForcedWarm = 2
