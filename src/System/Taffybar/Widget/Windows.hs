{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.Windows
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Menu widget that shows the title of the currently focused window and that,
-- when clicked, displays the list of all currently open windows allowing to
-- switch to any of them.
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

import           Control.Monad.Reader
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified Graphics.UI.Gtk as Gtk2hs
import           System.Taffybar.Compat.GtkLibs
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
truncatedGetMenuLabel maxLength =
  fmap (Gtk2hs.escapeMarkup . truncateString maxLength) .
  runX11Def "(nameless window)" . getWindowTitle

truncatedGetActiveLabel :: Int -> TaffyIO String
truncatedGetActiveLabel maxLength =
  truncateString maxLength <$> runX11Def "(nameless window)" getActiveWindowTitle

defaultWindowsConfig :: WindowsConfig
defaultWindowsConfig =
  WindowsConfig
  { getMenuLabel = truncatedGetMenuLabel 35
  , getActiveLabel = truncatedGetActiveLabel 35
  }

-- | Create a new Windows widget that will use the given Pager as
-- its source of events.
windowsNew :: WindowsConfig -> TaffyIO Gtk2hs.Widget
windowsNew config = (`widgetSetClass` "Windows") =<< fromGIWidget =<< do
  label <- lift $ do
    label <- Gtk.labelNew Nothing
    Gtk.widgetSetName label "label"
    return label

  let setLabelTitle title = lift $ runOnUIThread $ Gtk.labelSetMarkup label (T.pack title)
      activeWindowUpdatedCallback _ = getActiveLabel config >>= setLabelTitle

  subscription <- subscribeToEvents ["_NET_ACTIVE_WINDOW"] activeWindowUpdatedCallback
  _ <- liftReader (Gtk.onWidgetUnrealize label) (unsubscribe subscription)

  context <- ask

  labelWidget <- Gtk.toWidget label
  dynamicMenuNew
    DynamicMenuConfig { dmClickWidget = labelWidget
                      , dmPopulateMenu = flip runReaderT context . (fillMenu config)
                      }

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
