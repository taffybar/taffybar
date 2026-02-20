{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

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
module System.Taffybar.Widget.Windows where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Default (Default (..))
import Data.Maybe
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import System.Taffybar.Context
import System.Taffybar.Information.EWMHDesktopInfo
import System.Taffybar.Util
import System.Taffybar.Widget.Generic.DynamicMenu
import System.Taffybar.Widget.Generic.ScalingImage (scalingImage)
import System.Taffybar.Widget.Util
import System.Taffybar.Widget.Workspaces.Legacy.EWMH (WindowIconPixbufGetter, defaultGetWindowIconPixbuf, getWindowData)

-- | Behavior configuration for the windows menu widget.
data WindowsConfig = WindowsConfig
  { -- | A monadic function that will be used to make a label for the window in
    -- the window menu.
    getMenuLabel :: X11Window -> TaffyIO T.Text,
    -- | Action to build the label text for the active window.
    getActiveLabel :: TaffyIO T.Text,
    -- | Optional function to retrieve a pixbuf to show next to the
    -- window label.
    getActiveWindowIconPixbuf :: Maybe WindowIconPixbufGetter
  }

-- | Default menu-label renderer using the X11 window title.
defaultGetMenuLabel :: X11Window -> TaffyIO T.Text
defaultGetMenuLabel window = do
  windowString <- runX11Def "(nameless window)" (getWindowTitle window)
  return $ T.pack windowString

-- | Default active-window label renderer.
defaultGetActiveLabel :: TaffyIO T.Text
defaultGetActiveLabel = do
  fromMaybe ""
    <$> ( runX11Def Nothing getActiveWindow
            >>= traverse defaultGetMenuLabel
        )

-- | Truncate the active-window label to a maximum length.
truncatedGetActiveLabel :: Int -> TaffyIO T.Text
truncatedGetActiveLabel maxLength =
  truncateText maxLength <$> defaultGetActiveLabel

-- | Truncate window labels in the popup menu to a maximum length.
truncatedGetMenuLabel :: Int -> X11Window -> TaffyIO T.Text
truncatedGetMenuLabel maxLength =
  fmap (truncateText maxLength) . defaultGetMenuLabel

-- | Default configuration used by 'windowsNew'.
defaultWindowsConfig :: WindowsConfig
defaultWindowsConfig =
  WindowsConfig
    { getMenuLabel = truncatedGetMenuLabel 35,
      getActiveLabel = truncatedGetActiveLabel 35,
      getActiveWindowIconPixbuf = Just defaultGetWindowIconPixbuf
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
      refresh = refreshLabel >> lift refreshIcon

  subscription <-
    subscribeToPropertyEvents
      [ewmhActiveWindow, ewmhWMName, ewmhWMClass]
      (const refresh)

  void $ mapReaderT (Gtk.onWidgetUnrealize hbox) (unsubscribe subscription)
  void refresh

  Gtk.widgetShowAll hbox
  boxWidget <- Gtk.toWidget hbox

  runTaffy <- asks (flip runReaderT)
  menu <-
    dynamicMenuNew
      DynamicMenuConfig
        { dmClickWidget = boxWidget,
          dmPopulateMenu = runTaffy . fillMenu config
        }

  widgetSetClassGI menu "windows"

-- | Build the active-window label and return an update action for it.
buildWindowsLabel :: TaffyIO (T.Text -> IO (), Gtk.Widget)
buildWindowsLabel = do
  label <- lift $ Gtk.labelNew Nothing
  lift $ Gtk.labelSetSingleLineMode label True
  lift $ Gtk.labelSetEllipsize label Pango.EllipsizeModeEnd
  let setLabelTitle title = postGUIASync $ Gtk.labelSetText label title
  (setLabelTitle,) <$> Gtk.toWidget label

-- | Build the active-window icon and return an update action for it.
buildWindowsIcon :: WindowIconPixbufGetter -> TaffyIO (IO (), Gtk.Widget)
buildWindowsIcon windowIconPixbufGetter = do
  runTaffy <- asks (flip runReaderT)
  let getActiveWindowPixbuf size = runTaffy . runMaybeT $ do
        wd <-
          MaybeT $
            runX11Def Nothing $
              traverse (getWindowData Nothing []) =<< getActiveWindow
        MaybeT $ windowIconPixbufGetter size wd

  (imageWidget, updateImage) <- scalingImage getActiveWindowPixbuf Gtk.OrientationHorizontal
  return (postGUIASync updateImage, imageWidget)

-- | Populate the given menu widget with the list of all currently open windows.
fillMenu :: (Gtk.IsMenuShell a) => WindowsConfig -> a -> ReaderT Context IO ()
fillMenu config menu =
  ask >>= \context ->
    runX11Def () $ do
      windowIds <- getWindows
      forM_ windowIds $ \windowId ->
        lift $ do
          labelText <- runReaderT (getMenuLabel config windowId) context
          let focusCallback =
                runReaderT (runX11 $ focusWindow windowId) context
                  >> return True
          item <- Gtk.menuItemNewWithLabel labelText
          _ <- Gtk.onWidgetButtonPressEvent item $ const focusCallback
          Gtk.menuShellAppend menu item
          Gtk.widgetShow item
