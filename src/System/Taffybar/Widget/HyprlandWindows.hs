{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.HyprlandWindows
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Menu widget that shows the title of the currently focused Hyprland window
-- and that, when clicked, displays a menu from which the user may select a
-- window to which to switch focus.
module System.Taffybar.Widget.HyprlandWindows where

import Control.Concurrent (killThread)
import Control.Monad (forM_, void)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Default (Default (..))
import Data.List (find)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import System.Log.Logger (Priority (..))
import System.Taffybar.Context
import System.Taffybar.Hyprland (runHyprlandCommandRawT)
import qualified System.Taffybar.Information.Hyprland as Hypr
import System.Taffybar.Util
import System.Taffybar.Widget.Generic.DynamicMenu
import System.Taffybar.Widget.Generic.ScalingImage (scalingImage)
import System.Taffybar.Widget.HyprlandWorkspaces
  ( HyprlandClient,
    HyprlandWindow (..),
    HyprlandWindowIconPixbufGetter,
    defaultHyprlandGetWindowIconPixbuf,
    getActiveWindowAddress,
    runHyprctlJson,
    windowFromClient,
  )
import System.Taffybar.Widget.Util

-- | Window menu widget configuration for Hyprland.
data HyprlandWindowsConfig = HyprlandWindowsConfig
  { -- | A monadic function used to build labels for windows in the menu.
    getMenuLabel :: HyprlandWindow -> TaffyIO T.Text,
    -- | Action to build the label text for the active window.
    getActiveLabel :: Maybe HyprlandWindow -> TaffyIO T.Text,
    -- | Optional function to retrieve a pixbuf to show next to the window label.
    getActiveWindowIconPixbuf :: Maybe HyprlandWindowIconPixbufGetter,
    updateIntervalSeconds :: Double
  }

-- | Build a menu label from a window title, truncating to a maximum length.
truncatedGetMenuLabel :: Int -> HyprlandWindow -> TaffyIO T.Text
truncatedGetMenuLabel maxLength window =
  return $ truncateText maxLength (T.pack $ windowTitle window)

-- | Default menu label builder used by 'defaultHyprlandWindowsConfig'.
defaultGetMenuLabel :: HyprlandWindow -> TaffyIO T.Text
defaultGetMenuLabel = truncatedGetMenuLabel 35

-- | Default active-window label builder used by 'defaultHyprlandWindowsConfig'.
defaultGetActiveLabel :: Maybe HyprlandWindow -> TaffyIO T.Text
defaultGetActiveLabel = maybe (return "") defaultGetMenuLabel

-- | Default configuration for 'hyprlandWindowsNew'.
defaultHyprlandWindowsConfig :: HyprlandWindowsConfig
defaultHyprlandWindowsConfig =
  HyprlandWindowsConfig
    { getMenuLabel = defaultGetMenuLabel,
      getActiveLabel = defaultGetActiveLabel,
      getActiveWindowIconPixbuf = Just defaultHyprlandGetWindowIconPixbuf,
      updateIntervalSeconds = 1
    }

instance Default HyprlandWindowsConfig where
  def = defaultHyprlandWindowsConfig

-- | Create a new Hyprland Windows widget.
hyprlandWindowsNew :: HyprlandWindowsConfig -> TaffyIO Gtk.Widget
hyprlandWindowsNew config = do
  hbox <- lift $ Gtk.boxNew Gtk.OrientationHorizontal 0

  refreshIcon <- case getActiveWindowIconPixbuf config of
    Just getIcon -> do
      (rf, icon) <- buildWindowsIcon getIcon
      Gtk.boxPackStart hbox icon True True 0
      pure rf
    Nothing -> pure (pure ())

  (setLabelTitle, label) <- buildWindowsLabel
  Gtk.boxPackStart hbox label True True 0
  let refreshLabel = do
        activeWindow <- getActiveHyprlandWindow
        labelText <- getActiveLabel config activeWindow
        lift $ setLabelTitle labelText

  let refresh = refreshLabel >> lift refreshIcon
  ctx <- ask
  void refresh
  threadId <-
    lift $
      foreverWithDelay (updateIntervalSeconds config) $
        void $
          runReaderT refresh ctx

  _ <- lift $ Gtk.onWidgetUnrealize hbox $ killThread threadId

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

-- | Create the label widget used to show the active window title.
buildWindowsLabel :: TaffyIO (T.Text -> IO (), Gtk.Widget)
buildWindowsLabel = do
  label <- lift $ Gtk.labelNew Nothing
  lift $ Gtk.labelSetSingleLineMode label True
  lift $ Gtk.labelSetEllipsize label Pango.EllipsizeModeEnd
  let setLabelTitle title = postGUIASync $ Gtk.labelSetText label title
  (setLabelTitle,) <$> Gtk.toWidget label

-- | Create the icon widget used to show the active window icon.
buildWindowsIcon :: HyprlandWindowIconPixbufGetter -> TaffyIO (IO (), Gtk.Widget)
buildWindowsIcon windowIconPixbufGetter = do
  runTaffy <- asks (flip runReaderT)
  let getActiveWindowPixbuf size = runTaffy . runMaybeT $ do
        wd <- MaybeT getActiveHyprlandWindow
        MaybeT $ windowIconPixbufGetter size wd

  (imageWidget, updateImage) <- scalingImage getActiveWindowPixbuf Gtk.OrientationHorizontal
  return (postGUIASync updateImage, imageWidget)

-- | Retrieve the currently active Hyprland window, if one is known.
getActiveHyprlandWindow :: TaffyIO (Maybe HyprlandWindow)
getActiveHyprlandWindow = find windowActive <$> getHyprlandWindows

-- | Retrieve all Hyprland windows currently reported by @hyprctl@.
getHyprlandWindows :: TaffyIO [HyprlandWindow]
getHyprlandWindows = do
  activeAddr <- getActiveWindowAddress
  clientsResult <- runHyprctlJson ["-j", "clients"]
  case clientsResult of
    Left err ->
      logPrintF
        "System.Taffybar.Widget.HyprlandWindows"
        WARNING
        "hyprctl clients failed: %s"
        err
        >> return []
    Right clients ->
      return $ map (windowFromClient activeAddr) (clients :: [HyprlandClient])

-- | Populate the dynamic menu with the current Hyprland windows.
fillMenu :: (Gtk.IsMenuShell a) => HyprlandWindowsConfig -> a -> ReaderT Context IO ()
fillMenu config menu =
  ask >>= \context -> do
    windowIds <- getHyprlandWindows
    forM_ windowIds $ \windowData ->
      lift $ do
        labelText <- runReaderT (getMenuLabel config windowData) context
        let focusCallback =
              runReaderT (focusHyprlandWindow windowData) context
                >> return True
        item <- Gtk.menuItemNewWithLabel labelText
        _ <- Gtk.onWidgetButtonPressEvent item $ const focusCallback
        Gtk.menuShellAppend menu item
        Gtk.widgetShow item

-- | Focus the supplied window using Hyprland's @dispatch focuswindow@ command.
focusHyprlandWindow :: HyprlandWindow -> TaffyIO ()
focusHyprlandWindow windowData = do
  result <-
    runHyprlandCommandRawT $
      Hypr.hyprCommand
        [ "dispatch",
          "focuswindow",
          "address:" <> T.unpack (windowAddress windowData)
        ]
  case result of
    Left err ->
      logPrintF
        "System.Taffybar.Widget.HyprlandWindows"
        WARNING
        "Failed to focus window: %s"
        (show err)
    Right _ -> return ()
