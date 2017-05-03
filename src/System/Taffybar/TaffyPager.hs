{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.TaffyPager
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides a drop-in replacement for the XMonadLog widget
-- that allows to:
--
-- * click on a workspace label to switch to that workspace,
--
-- * left-click on the layout label to switch to the next layout, and
-- right-click to switch to the first layout,
--
-- * click on the window title to pop-up a list of all the currently open
-- windows that can be clicked to switch to any of them,
--
-- All its interactions with the windows manager are performed via EWMH
-- hints and X11 events.
--
-- This widget is actually only a convenience wrapper around a Pager, a
-- WorkspaceSwitcher, a LayoutSwitcher and a WindowSwitcher. If you are
-- looking for more advanced configurations (like having components
-- displayed separately, or using only part of them), consult directly the
-- documentation for each of the components.
--
-----------------------------------------------------------------------------

module System.Taffybar.TaffyPager (
  -- * Usage
  -- $usage
  PagerConfig (..)
, defaultPagerConfig
, taffyPagerHUDLegacy
, taffyPagerHUDNew
, taffyPagerNew
) where

import Graphics.UI.Gtk
import System.Taffybar.LayoutSwitcher
import System.Taffybar.Pager
import System.Taffybar.WindowSwitcher
import System.Taffybar.WorkspaceHUD
import System.Taffybar.WorkspaceSwitcher

-- $usage
--
-- This widget requires that two hooks be installed in your @xmonad.hs@
-- configuration: EwmhDesktops from the XMonadContrib project, and the one
-- provided in the "System.Taffybar.Hooks.PagerHints" module:
--
-- > import XMonad.Hooks.EwmhDesktops (ewmh)
-- > import System.Taffybar.Hooks.PagerHints (pagerHints)
-- > main = do
-- >   xmonad $ ewmh $ pagerHints $ defaultConfig
-- > ...
--
-- That's all: no log hooks, no urgency hooks, no DBus client. Once you've
-- configured @xmonad.hs@, you can use the widget in your @taffybar.hs@
-- file:
--
-- > import System.Taffybar.TaffyPager
-- > main = do
-- >   let pager = taffyPagerNew defaultPagerConfig
--
-- now you can use @pager@ as any other Taffybar widget.

-- | Create a new TaffyPager widget.
taffyPagerNew :: PagerConfig -> IO Widget
taffyPagerNew cfg = do
  pgr <- pagerNew cfg
  wss <- wspaceSwitcherNew pgr
  los <- layoutSwitcherNew pgr
  wnd <- windowSwitcherNew pgr
  sp1 <- separator cfg
  sp2 <- separator cfg
  box <- hBoxNew False 0

  boxPackStart box wss PackNatural 0
  boxPackStart box sp1 PackNatural 0
  boxPackStart box los PackNatural 0
  boxPackStart box sp2 PackNatural 0
  boxPackStart box wnd PackNatural 0

  widgetShowAll box
  return (toWidget box)

taffyPagerHUDNew :: PagerConfig -> WorkspaceHUDConfig -> IO Widget
taffyPagerHUDNew cfg hudConfig = do
  pgr <- pagerNew cfg
  whud <- buildWorkspaceHUD hudConfig pgr
  los <- layoutSwitcherNew pgr
  wnd <- windowSwitcherNew pgr
  sp1 <- separator cfg
  sp2 <- separator cfg
  box <- hBoxNew False 0

  boxPackStart box whud PackNatural 0
  boxPackStart box sp1 PackNatural 0
  boxPackStart box los PackNatural 0
  boxPackStart box sp2 PackNatural 0
  boxPackStart box wnd PackNatural 0

  widgetShowAll box
  return (toWidget box)

taffyPagerHUDLegacy :: PagerConfig -> IO Widget
taffyPagerHUDLegacy cfg = do
  pgr <- pagerNew cfg
  whud <- buildWorkspaceHUD (hudFromPagerConfig cfg) pgr
  los <- layoutSwitcherNew pgr
  wnd <- windowSwitcherNew pgr
  sp1 <- separator cfg
  sp2 <- separator cfg
  box <- hBoxNew False 0

  boxPackStart box whud PackNatural 0
  boxPackStart box sp1 PackNatural 0
  boxPackStart box los PackNatural 0
  boxPackStart box sp2 PackNatural 0
  boxPackStart box wnd PackNatural 0

  widgetShowAll box
  return (toWidget box)


-- | Create a new separator label to put between two sub-components.
separator :: PagerConfig -> IO Label
separator cfg = do
  sep <- labelNew (Nothing :: Maybe String)
  labelSetMarkup sep (widgetSep cfg)
  return sep
