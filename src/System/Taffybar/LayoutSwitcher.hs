{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.LayoutSwitcher
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple text widget that shows the XMonad layout used in the currently
-- active workspace, and that allows to change it by clicking with the
-- mouse: left-click to switch to the next layout in the list, right-click
-- to switch to the first one (as configured in @xmonad.hs@)
--
-- N.B. If you're just looking for a drop-in replacement for the
-- "System.Taffybar.XMonadLog" widget that is clickable and doesn't require
-- DBus, you may want to see first "System.Taffybar.TaffyPager".
--
-----------------------------------------------------------------------------

module System.Taffybar.LayoutSwitcher (
  -- * Usage
  -- $usage
  layoutSwitcherNew
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.X11.Xlib.Extras (Event)
import System.Taffybar.Pager
import System.Information.X11DesktopInfo
import System.Taffybar.Widgets.Util

-- $usage
--
-- This widget requires that the "System.Taffybar.Hooks.PagerHints" hook be
-- installed in your @xmonad.hs@:
--
-- > import System.Taffybar.Hooks.PagerHints (pagerHints)
-- > main = do
-- >   xmonad $ ewmh $ pagerHints $ defaultConfig
-- > ...
--
-- Once you've properly configured @xmonad.hs@, you can use the widget in
-- your @taffybar.hs@ file:
--
-- > import System.Taffybar.LayoutSwitcher
-- > main = do
-- >   pager <- pagerNew defaultPagerConfig
-- >   let los = layoutSwitcherNew pager
--
-- now you can use @los@ as any other Taffybar widget.

-- | Name of the X11 events to subscribe, and of the hint to look for for
-- the name of the current layout.
xLayoutProp :: String
xLayoutProp = "_XMONAD_CURRENT_LAYOUT"

-- | Create a new LayoutSwitcher widget that will use the given Pager as
-- its source of events.
layoutSwitcherNew :: Pager -> IO Gtk.Widget
layoutSwitcherNew pager = do
  label <- Gtk.labelNew (Nothing :: Maybe String)
  -- This callback is run in a separate thread and needs to use
  -- postGUIAsync
  let cfg = config pager
      callback = pagerCallback cfg label
  subscribe pager callback xLayoutProp
  assembleWidget label

-- | Build a suitable callback function that can be registered as Listener
-- of "_XMONAD_CURRENT_LAYOUT" custom events. These events are emitted by
-- the PagerHints hook to notify of changes in the current layout.
pagerCallback :: PagerConfig -> Gtk.Label -> Event -> IO ()
pagerCallback cfg label _ = Gtk.postGUIAsync $ do
  layout <- withDefaultCtx $ readAsString Nothing xLayoutProp
  layoutMarkup <- activeLayoutIO cfg $ activeLayout cfg layout
  Gtk.labelSetMarkup label layoutMarkup

-- | Build the graphical representation of the widget.
assembleWidget :: Gtk.Label -> IO Gtk.Widget
assembleWidget label = do
  ebox <- Gtk.eventBoxNew
  Gtk.containerAdd ebox label
  _ <- Gtk.on ebox Gtk.buttonPressEvent dispatchButtonEvent
  Gtk.widgetShowAll ebox
  return $ Gtk.toWidget ebox

-- | Call 'switch' with the appropriate argument (1 for left click, -1 for
-- right click), depending on the click event received.
dispatchButtonEvent :: Gtk.EventM Gtk.EButton Bool
dispatchButtonEvent = do
  btn <- Gtk.eventButton
  let trigger = onClick [Gtk.SingleClick]
  case btn of
    Gtk.LeftButton  -> trigger $ switch 1
    Gtk.RightButton -> trigger $ switch (-1)
    _               -> return False

-- | Emit a new custom event of type _XMONAD_CURRENT_LAYOUT, that can be
-- intercepted by the PagerHints hook, which in turn can instruct XMonad to
-- switch to a different layout.
switch :: (MonadIO m) => Int -> m ()
switch n = liftIO $ withDefaultCtx $ do
  cmd <- getAtom xLayoutProp
  sendCommandEvent cmd (fromIntegral n)
