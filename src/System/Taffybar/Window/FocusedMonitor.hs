{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Window.FocusedMonitor
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Focused-monitor CSS class updates for bar windows.
module System.Taffybar.Window.FocusedMonitor
  ( FocusedMonitorHooks (..),
    setupFocusedMonitorClassUpdates,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM.TChan (TChan, readTChan)
import Control.Monad (forever, when)
import Control.Monad.STM (atomically)
import Data.Int (Int32)
import qualified Data.Text as T
import Data.Unique (Unique)
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Util (updateWidgetClasses)

data FocusedMonitorHooks
  = FocusedMonitorHooksX11
      { resolveFocusedMonitorX11 :: IO (Maybe Gdk.Monitor),
        subscribeToFocusedMonitorX11Events :: IO () -> IO Unique,
        unsubscribeFromFocusedMonitorX11Events :: Unique -> IO ()
      }
  | FocusedMonitorHooksWayland
      { resolveFocusedMonitorWayland :: IO (Maybe Gdk.Monitor),
        getFocusedMonitorHyprlandEvents :: IO (TChan T.Text)
      }

focusedMonitorClasses :: [T.Text]
focusedMonitorClasses = ["focused-monitor", "unfocused-monitor"]

focusedMonitorClass :: Bool -> T.Text
focusedMonitorClass True = "focused-monitor"
focusedMonitorClass False = "unfocused-monitor"

getBarMonitor :: Gtk.Window -> Maybe Int32 -> IO (Maybe Gdk.Monitor)
getBarMonitor window maybeMonitorNumber = do
  display <- Gtk.widgetGetDisplay window
  maybe
    (Gdk.displayGetPrimaryMonitor display)
    (Gdk.displayGetMonitor display)
    maybeMonitorNumber

monitorsMatch :: Gdk.Monitor -> Gdk.Monitor -> IO Bool
monitorsMatch left right = do
  leftGeometry <- Gdk.getMonitorGeometry left
  rightGeometry <- Gdk.getMonitorGeometry right
  case (leftGeometry, rightGeometry) of
    (Nothing, Nothing) -> return True
    (Just leftRect, Just rightRect) -> Gdk.rectangleEqual leftRect rightRect
    _ -> return False

updateFocusedMonitorClass ::
  IO (Maybe Gdk.Monitor) ->
  Gtk.Window ->
  Maybe Int32 ->
  IO ()
updateFocusedMonitorClass resolveFocusedMonitor window maybeBarMonitorNumber = do
  maybeFocusedMonitor <- resolveFocusedMonitor
  maybeBarMonitor <- getBarMonitor window maybeBarMonitorNumber
  isFocused <- case (maybeFocusedMonitor, maybeBarMonitor) of
    (Just focusedMonitor, Just barMonitor) ->
      monitorsMatch focusedMonitor barMonitor
    _ -> return False
  updateWidgetClasses
    window
    [focusedMonitorClass isFocused]
    focusedMonitorClasses

isRelevantFocusedMonitorHyprlandEvent :: T.Text -> Bool
isRelevantFocusedMonitorHyprlandEvent eventLine =
  let hyprEventName = T.takeWhile (/= '>') eventLine
   in hyprEventName
        `elem` [ "workspace",
                 "workspacev2",
                 "focusedmon",
                 "activewindow",
                 "activewindowv2",
                 "monitoradded",
                 "monitorremoved",
                 "taffybar-hyprland-connected"
               ]

setupFocusedMonitorClassUpdates ::
  FocusedMonitorHooks ->
  Gtk.Window ->
  Maybe Int32 ->
  IO ()
setupFocusedMonitorClassUpdates hooks window maybeBarMonitorNumber = do
  let resolveFocusedMonitor = case hooks of
        FocusedMonitorHooksX11 {resolveFocusedMonitorX11 = resolve} -> resolve
        FocusedMonitorHooksWayland {resolveFocusedMonitorWayland = resolve} -> resolve
      refresh =
        postGUIASync $
          updateFocusedMonitorClass
            resolveFocusedMonitor
            window
            maybeBarMonitorNumber
  case hooks of
    FocusedMonitorHooksX11
      { subscribeToFocusedMonitorX11Events = subscribe,
        unsubscribeFromFocusedMonitorX11Events = unsubscribe
      } -> do
        subscription <- subscribe refresh
        _ <- Gtk.onWidgetUnrealize window $ unsubscribe subscription
        return ()
    FocusedMonitorHooksWayland
      { getFocusedMonitorHyprlandEvents = getEvents
      } -> do
        events <- getEvents
        tid <- forkIO $ forever $ do
          eventLine <- atomically $ readTChan events
          when (isRelevantFocusedMonitorHyprlandEvent eventLine) refresh
        _ <- Gtk.onWidgetUnrealize window $ killThread tid
        return ()
  refresh
