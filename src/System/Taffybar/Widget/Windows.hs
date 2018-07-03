{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.Windows
-- Copyright   : (c) Ivan Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Menu widget that shows the title of the currently focused window and that,
-- when clicked, displays a menu from which the user may select a window to
-- which to switch the focus.
-----------------------------------------------------------------------------

module System.Taffybar.Widget.Windows (
  -- * Usage
  -- $usage
    windowsNew
  , WindowsConfig(..)
  , defaultWindowsConfig
  , truncatedGetActiveLabel
  , truncatedGetMenuLabel
) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.GI.Gtk.Threading
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import           GI.GLib (markupEscapeText)
import           System.Taffybar.Context
import           System.Taffybar.Information.EWMHDesktopInfo
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.DynamicMenu
import           System.Taffybar.Widget.Util

-- $usage
--
-- The window switcher widget requires that the EwmhDesktops hook from the
-- XMonadContrib project be installed in your @xmonad.hs@ file:
--
-- > import XMonad.Hooks.EwmhDesktops (ewmh)
-- > main = do
-- >   xmonad $ ewmh $ defaultConfig
-- > ...

data WindowsConfig = WindowsConfig
  { getMenuLabel :: X11Window -> TaffyIO String
  -- ^ A monadic function that will be used to make a label for the window in
  -- the window menu.
  , getActiveLabel :: TaffyIO String
  -- ^ Action to build the label text for the active window.
  }

truncatedGetMenuLabel :: Int -> X11Window -> TaffyIO String
truncatedGetMenuLabel maxLength window =
  runX11Def "(nameless window)" (getWindowTitle window) >>= \s ->
  T.unpack <$> markupEscapeText (T.pack (truncateString maxLength s)) (fromIntegral $ maxLength)

truncatedGetActiveLabel :: Int -> TaffyIO String
truncatedGetActiveLabel maxLength =
  runX11Def "(nameless window)" getActiveWindowTitle >>= \s ->
  T.unpack <$> markupEscapeText (T.pack (truncateString maxLength s)) (fromIntegral $ maxLength)

defaultWindowsConfig :: WindowsConfig
defaultWindowsConfig =
  WindowsConfig
  { getMenuLabel = truncatedGetMenuLabel 35
  , getActiveLabel = truncatedGetActiveLabel 35
  }

-- | Create a new Windows widget that will use the given Pager as
-- its source of events.
windowsNew :: WindowsConfig -> TaffyIO Gtk.Widget
windowsNew config = do
  label <- lift $ Gtk.labelNew Nothing

  let setLabelTitle title = lift $ postGUIASync $ Gtk.labelSetMarkup label (T.pack title)
      activeWindowUpdatedCallback _ = getActiveLabel config >>= setLabelTitle

  subscription <- subscribeToEvents ["_NET_ACTIVE_WINDOW"] activeWindowUpdatedCallback
  _ <- liftReader (Gtk.onWidgetUnrealize label) (unsubscribe subscription)

  context <- ask

  labelWidget <- Gtk.toWidget label
  menu <- dynamicMenuNew
    DynamicMenuConfig { dmClickWidget = labelWidget
                      , dmPopulateMenu = flip runReaderT context . fillMenu config
                      }

  widgetSetClassGI menu "windows"

-- | Populate the given menu widget with the list of all currently open windows.
fillMenu :: Gtk.IsMenuShell a => WindowsConfig -> a -> ReaderT Context IO ()
fillMenu config menu = ask >>= \context ->
  runX11Def () $ do
    windowIds <- getWindows
    forM_ windowIds $ \windowId ->
      lift $ do
        labelText <- runReaderT (getMenuLabel config windowId) context
        let focusCallback = runReaderT (runX11 $ focusWindow windowId) context >> return True
        item <- Gtk.menuItemNewWithLabel $ T.pack labelText
        _ <- Gtk.onWidgetButtonPressEvent item $ const focusCallback
        Gtk.menuShellAppend menu item
        Gtk.widgetShow item
