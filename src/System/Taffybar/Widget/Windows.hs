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
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Maybe
import           Data.Default (Default(..))
import           Data.Maybe
import qualified Data.Text as T
import           GI.GLib (markupEscapeText)
import qualified GI.Gtk as Gtk
import           System.Taffybar.Context
import           System.Taffybar.Information.EWMHDesktopInfo
import           System.Taffybar.Widget.Generic.AutoSizeImage
import           System.Taffybar.Widget.Generic.DynamicMenu
import           System.Taffybar.Widget.Util
import           System.Taffybar.Widget.Workspaces (WindowIconPixbufGetter, getWindowData, defaultGetWindowIconPixbuf)
import           System.Taffybar.Util

data WindowsConfig = WindowsConfig
  { getMenuLabel :: X11Window -> TaffyIO T.Text
  -- ^ A monadic function that will be used to make a label for the window in
  -- the window menu.
  , getActiveLabel :: TaffyIO T.Text
  -- ^ Action to build the label text for the active window.
  , getActiveWindowIconPixbuf :: Maybe WindowIconPixbufGetter
  -- ^ Optional function to retrieve a pixbuf to show next to the
  -- window label.
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
  , getActiveWindowIconPixbuf = Just defaultGetWindowIconPixbuf
  }

instance Default WindowsConfig where
  def = defaultWindowsConfig

-- | Create a new Windows widget that will use the given Pager as
-- its source of events.
windowsNew :: WindowsConfig -> TaffyIO Gtk.Widget
windowsNew config = do
  hbox <- lift $ Gtk.boxNew Gtk.OrientationHorizontal 0

  refreshIcon <- case getActiveWindowIconPixbuf config of
    Just getIcon -> do
      (rf, icon) <- buildWindowsIcon getIcon
      Gtk.boxPackStart hbox icon True True 0
      pure rf
    Nothing -> pure (pure ())

  (setLabelTitle, label) <- buildWindowsLabel
  Gtk.boxPackStart hbox label True True 0
  let refreshLabel = getActiveLabel config >>= lift . setLabelTitle

  subscription <- subscribeToPropertyEvents
    [ewmhActiveWindow, ewmhWMName, ewmhWMClass]
    (const $ refreshLabel >> lift refreshIcon)

  void $ mapReaderT (Gtk.onWidgetUnrealize hbox) (unsubscribe subscription)

  Gtk.widgetShowAll hbox
  boxWidget <- Gtk.toWidget hbox

  context <- ask
  menu <- dynamicMenuNew
    DynamicMenuConfig { dmClickWidget = boxWidget
                      , dmPopulateMenu = runTaffy context . fillMenu config
                      }

  widgetSetClassGI menu "windows"

buildWindowsLabel :: TaffyIO (T.Text -> IO (), Gtk.Widget)
buildWindowsLabel = do
  label <- lift $ Gtk.labelNew Nothing
  let setLabelTitle title = postGUIASync $ Gtk.labelSetMarkup label title
  (setLabelTitle,) <$> Gtk.toWidget label

buildWindowsIcon :: WindowIconPixbufGetter -> TaffyIO (IO (), Gtk.Widget)
buildWindowsIcon windowIconPixbufGetter = do
  icon <- lift Gtk.imageNew

  context <- ask
  let getActiveWindowPixbuf size = runTaffy context . runMaybeT $ do
        wd <- MaybeT $ runX11Def Nothing $
          traverse (getWindowData Nothing []) =<< getActiveWindow
        MaybeT $ windowIconPixbufGetter size wd

  updateImage <- autoSizeImage icon getActiveWindowPixbuf Gtk.OrientationHorizontal
  (postGUIASync updateImage,) <$> Gtk.toWidget icon

-- | Populate the given menu widget with the list of all currently open windows.
fillMenu :: Gtk.IsMenuShell a => WindowsConfig -> a -> ReaderT Context IO ()
fillMenu config menu = ask >>= \context ->
  runX11Def () $ do
    windowIds <- getWindows
    forM_ windowIds $ \windowId ->
      lift $ do
        labelText <- runTaffy context (getMenuLabel config windowId)
        let focusCallback = runTaffy context (runX11 $ focusWindow windowId) >>
                            return True
        item <- Gtk.menuItemNewWithLabel labelText
        _ <- Gtk.onWidgetButtonPressEvent item $ const focusCallback
        Gtk.menuShellAppend menu item
        Gtk.widgetShow item
