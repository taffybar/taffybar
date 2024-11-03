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

module System.Taffybar.Widget.Windows where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Default (Default(..))
import           Data.Maybe
import qualified Data.Text as T
import           GI.GLib (markupEscapeText)
import qualified GI.Gtk as Gtk
import           System.Taffybar.Context
import           System.Taffybar.Information.EWMHDesktopInfo
import           System.Taffybar.Information.SafeX11
import           System.Taffybar.Information.X11DesktopInfo
import           System.Taffybar.Widget.Generic.AutoSizeImage
import           System.Taffybar.Widget.Generic.DynamicMenu
import           System.Taffybar.Widget.Util
import           System.Taffybar.Widget.Workspaces
import           System.Taffybar.Util

data WindowsConfig = WindowsConfig
  { getMenuLabel :: X11Window -> TaffyIO T.Text
  -- ^ A monadic function that will be used to make a label for the window in
  -- the window menu.
  , getActiveLabel :: TaffyIO T.Text
  -- ^ Action to build the label text for the active window.
  , showActiveIcon :: Bool
  , getActiveWindowIconPixbuf :: WindowIconPixbufGetter
  }

defaultGetMenuLabel :: X11Window -> TaffyIO T.Text
defaultGetMenuLabel window = do
  windowString <- runX11Def "(nameless window)" (getWindowTitle window)
  return $ T.pack windowString

defaultGetActiveLabel :: TaffyIO T.Text
defaultGetActiveLabel = do
  label <- fromMaybe "" <$> (runX11Def Nothing getActiveWindow >>=
                                       traverse defaultGetMenuLabel)
  markupEscapeText label (-1)

truncatedGetActiveLabel :: Int -> TaffyIO T.Text
truncatedGetActiveLabel maxLength =
  truncateText maxLength <$> defaultGetActiveLabel

truncatedGetMenuLabel :: Int -> X11Window -> TaffyIO T.Text
truncatedGetMenuLabel maxLength =
  fmap (truncateText maxLength) . defaultGetMenuLabel

defaultWindowsConfig :: WindowsConfig
defaultWindowsConfig =
  WindowsConfig
  { getMenuLabel = truncatedGetMenuLabel 35
  , getActiveLabel = truncatedGetActiveLabel 35
  , showActiveIcon = True
  , getActiveWindowIconPixbuf = defaultGetWindowIconPixbuf
  }

instance Default WindowsConfig where
  def = defaultWindowsConfig

-- | Create a new Windows widget that will use the given Pager as
-- its source of events.
windowsNew :: WindowsConfig -> TaffyIO Gtk.Widget
windowsNew config = do
  label <- lift $ Gtk.labelNew Nothing

  icon <- lift $ Gtk.imageNew

  context <- ask

  let setLabelTitle title = lift $ postGUIASync $ Gtk.labelSetMarkup label title
      getActiveWindowData = getActiveWindow >>= (traverse $ getWindowData Nothing [])
      getThePixbuf size = fmap join $ (runX11Def Nothing getActiveWindowData) >>= (traverse $ getActiveWindowIconPixbuf config size)
      refreshImage = flip runReaderT context . getThePixbuf

  forceImageRefresh <- autoSizeImage icon refreshImage Gtk.OrientationHorizontal

  let activeWindowUpdatedCallback _ =
        (lift forceImageRefresh) >> getActiveLabel config >>= setLabelTitle

  subscription <-
    subscribeToPropertyEvents [ewmhActiveWindow, ewmhWMName, ewmhWMClass]
                      activeWindowUpdatedCallback
  _ <- mapReaderT (Gtk.onWidgetUnrealize label) (unsubscribe subscription)

  grid <- lift $ Gtk.gridNew

  let gridAttachWidgets showActiveIcon
        | showActiveIcon = (Gtk.gridAttach grid icon 0 0 1 1) >> (Gtk.gridAttach grid label 1 0 1 1)
        | otherwise = Gtk.gridAttach grid label 0 0 1 1

  gridAttachWidgets $ showActiveIcon config
  
  gridWidget <- Gtk.toWidget grid

  menu <- dynamicMenuNew
    DynamicMenuConfig { dmClickWidget = gridWidget
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
        let focusCallback = runReaderT (runX11 $ focusWindow windowId) context >>
                            return True
        item <- Gtk.menuItemNewWithLabel labelText
        _ <- Gtk.onWidgetButtonPressEvent item $ const focusCallback
        Gtk.menuShellAppend menu item
        Gtk.widgetShow item