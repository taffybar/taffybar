{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.WindowSwitcher
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Menu widget that shows the title of the currently focused window and that,
-- when clicked, displays the list of all currently open windows allowing to
-- switch to any of them.
--
-- N.B. If you're just looking for a drop-in replacement for the
-- "System.Taffybar.XMonadLog" widget that is clickable and doesn't require
-- DBus, you may want to see first "System.Taffybar.TaffyPager".
--
-----------------------------------------------------------------------------

module System.Taffybar.WindowSwitcher (
  -- * Usage
  -- $usage
  windowSwitcherNew
) where

import Control.Monad (forM_)
import Graphics.UI.Gtk
import Graphics.X11.Xlib.Extras (Event)
import System.Information.EWMHDesktopInfo
import System.Taffybar.Pager

-- $usage
--
-- This widget requires that the EwmhDesktops hook from the XMonadContrib
-- project be installed in your @xmonad.hs@ file:
--
-- > import XMonad.Hooks.EwmhDesktops (ewmh)
-- > main = do
-- >   xmonad $ ewmh $ defaultConfig
-- > ...
--
-- Once you've properly configured @xmonad.hs@, you can use the widget in
-- your @taffybar.hs@ file:
--
-- > import System.Taffybar.WindowSwitcher
-- > main = do
-- >   pager <- pagerNew defaultPagerConfig
-- >   let wnd = windowSwitcherNew pager
--
-- now you can use @wnd@ as any other Taffybar widget.

-- | Create a new WindowSwitcher widget that will use the given Pager as
-- its source of events.
windowSwitcherNew :: Pager -> IO Widget
windowSwitcherNew pager = do
  label <- labelNew (Nothing :: Maybe String)
  widgetSetName label "label"
  let cfg = config pager
      callback = pagerCallback cfg label
  subscribe pager callback "_NET_ACTIVE_WINDOW"
  assembleWidget label

-- | Build a suitable callback function that can be registered as Listener
-- of "_NET_ACTIVE_WINDOW" standard events. It will keep track of the
-- currently focused window.
pagerCallback :: PagerConfig -> Label -> Event -> IO ()
pagerCallback cfg label _ = do
  title <- withDefaultCtx getActiveWindowTitle
  let decorate = activeWindow cfg
  postGUIAsync $ labelSetMarkup label (decorate $ nonEmpty title)

-- | Build the graphical representation of the widget.
assembleWidget :: Label -> IO Widget
assembleWidget label = do
  title <- menuItemNew
  widgetSetName title "title"
  containerAdd title label

  switcher <- menuBarNew
  widgetSetName switcher "WindowSwitcher"
  containerAdd switcher title

  rcParseString $ unlines [ "style 'WindowSwitcher' {"
                          , "  xthickness = 0"
                          , "  GtkMenuBar::internal-padding = 0"
                          , "}"
                          , "style 'title' {"
                          , "  xthickness = 0"
                          , "  GtkMenuItem::horizontal-padding = 0"
                          , "}"
                          , "widget '*WindowSwitcher' style 'WindowSwitcher'"
                          , "widget '*WindowSwitcher*title' style 'title'"
                          ]
  menu <- menuNew
  widgetSetName menu "menu"

  menuTop <- widgetGetToplevel menu
  widgetSetName menuTop "Taffybar_WindowSwitcher"

  menuItemSetSubmenu title menu
  _ <- on title menuItemActivate $ fillMenu  menu
  _ <- on title menuItemDeselect $ emptyMenu menu

  widgetShowAll switcher
  return $ toWidget switcher

-- | Populate the given menu widget with the list of all currently open windows.
fillMenu :: MenuClass menu => menu -> IO ()
fillMenu menu = do
  handles  <- withDefaultCtx getWindowHandles
  if null handles then return () else do
    wsNames  <- withDefaultCtx getWorkspaceNames
    forM_ handles $ \handle -> do
      item <- menuItemNewWithLabel (formatEntry wsNames handle)
      _ <- onActivateLeaf item $ withDefaultCtx (focusWindow $ snd handle)
      menuShellAppend menu item
      widgetShow item

-- | Remove all contents from the given menu widget.
emptyMenu :: MenuClass menu => menu -> IO ()
emptyMenu menu = containerForeach menu $ \item ->
                 containerRemove menu item >> postGUIAsync (widgetDestroy item)

-- | Build the name to display in the list of windows by prepending the name
-- of the workspace it is currently in to the name of the window itself
formatEntry :: [String] -- ^ List of names of all available workspaces
            -> X11WindowHandle -- ^ Handle of the window to name
            -> String
formatEntry wsNames ((ws, wtitle, _), _) = wsName ++ ": " ++ (nonEmpty wtitle)
  where wsName = if 0 <= ws && ws < length wsNames
                 then wsNames !! ws
                 else "WS#" ++ show ws

-- | Return the given String if it's not empty, otherwise return "(nameless window)"
nonEmpty :: String -> String
nonEmpty x = case x of
               [] -> "(nameless window)"
               _  -> x
