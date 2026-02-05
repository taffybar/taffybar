{-# LANGUAGE OverloadedStrings #-}

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
-----------------------------------------------------------------------------

module System.Taffybar.Widget.HyprlandWindows where

import           Control.Concurrent (killThread)
import           Control.Monad (forM_, void)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Maybe
import           Data.Default (Default(..))
import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import           System.Log.Logger (Priority(..))
import           System.Taffybar.Context
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.AutoSizeImage
import           System.Taffybar.Widget.Generic.DynamicMenu
import           System.Taffybar.Widget.Util
import           System.Taffybar.Widget.HyprlandWorkspaces
  ( HyprlandWindow(..)
  , HyprlandWindowIconPixbufGetter
  , HyprlandClient
  , defaultHyprlandGetWindowIconPixbuf
  , getActiveWindowAddress
  , runHyprctlJson
  , windowFromClient
  )

-- | Window menu widget configuration for Hyprland.
data HyprlandWindowsConfig = HyprlandWindowsConfig
  { getMenuLabel :: HyprlandWindow -> TaffyIO T.Text
  -- ^ A monadic function used to build labels for windows in the menu.
  , getActiveLabel :: Maybe HyprlandWindow -> TaffyIO T.Text
  -- ^ Action to build the label text for the active window.
  , getActiveWindowIconPixbuf :: Maybe HyprlandWindowIconPixbufGetter
  -- ^ Optional function to retrieve a pixbuf to show next to the window label.
  , updateIntervalSeconds :: Double
  }

truncatedGetMenuLabel :: Int -> HyprlandWindow -> TaffyIO T.Text
truncatedGetMenuLabel maxLength window =
  return $ truncateText maxLength (T.pack $ windowTitle window)

defaultGetMenuLabel :: HyprlandWindow -> TaffyIO T.Text
defaultGetMenuLabel = truncatedGetMenuLabel 35

defaultGetActiveLabel :: Maybe HyprlandWindow -> TaffyIO T.Text
defaultGetActiveLabel = maybe (return "") defaultGetMenuLabel

defaultHyprlandWindowsConfig :: HyprlandWindowsConfig
defaultHyprlandWindowsConfig =
  HyprlandWindowsConfig
  { getMenuLabel = defaultGetMenuLabel
  , getActiveLabel = defaultGetActiveLabel
  , getActiveWindowIconPixbuf = Just defaultHyprlandGetWindowIconPixbuf
  , updateIntervalSeconds = 1
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
  threadId <- lift $ foreverWithDelay (updateIntervalSeconds config) $
    void $ runReaderT refresh ctx

  _ <- lift $ Gtk.onWidgetUnrealize hbox $ killThread threadId

  Gtk.widgetShowAll hbox
  boxWidget <- Gtk.toWidget hbox

  runTaffy <- asks (flip runReaderT)
  menu <- dynamicMenuNew
    DynamicMenuConfig { dmClickWidget = boxWidget
                      , dmPopulateMenu = runTaffy . fillMenu config
                      }

  widgetSetClassGI menu "windows"

buildWindowsLabel :: TaffyIO (T.Text -> IO (), Gtk.Widget)
buildWindowsLabel = do
  label <- lift $ Gtk.labelNew Nothing
  let setLabelTitle title = postGUIASync $ Gtk.labelSetMarkup label title
  (setLabelTitle,) <$> Gtk.toWidget label

buildWindowsIcon :: HyprlandWindowIconPixbufGetter -> TaffyIO (IO (), Gtk.Widget)
buildWindowsIcon windowIconPixbufGetter = do
  icon <- lift Gtk.imageNew

  runTaffy <- asks (flip runReaderT)
  let getActiveWindowPixbuf size = runTaffy . runMaybeT $ do
        wd <- MaybeT getActiveHyprlandWindow
        MaybeT $ windowIconPixbufGetter size wd

  updateImage <- autoSizeImage icon getActiveWindowPixbuf Gtk.OrientationHorizontal
  (postGUIASync updateImage,) <$> Gtk.toWidget icon

getActiveHyprlandWindow :: TaffyIO (Maybe HyprlandWindow)
getActiveHyprlandWindow = do
  windows <- getHyprlandWindows
  return $ listToMaybe $ filter windowActive windows

getHyprlandWindows :: TaffyIO [HyprlandWindow]
getHyprlandWindows = do
  activeAddr <- getActiveWindowAddress
  clientsResult <- runHyprctlJson ["-j", "clients"]
  case clientsResult of
    Left err ->
      logPrintF "System.Taffybar.Widget.HyprlandWindows" WARNING
        "hyprctl clients failed: %s" err >>
      return []
    Right clients ->
      return $ map (windowFromClient activeAddr) (clients :: [HyprlandClient])

fillMenu :: Gtk.IsMenuShell a => HyprlandWindowsConfig -> a -> ReaderT Context IO ()
fillMenu config menu = ask >>= \context -> do
  windowIds <- getHyprlandWindows
  forM_ windowIds $ \windowData ->
    lift $ do
      labelText <- runReaderT (getMenuLabel config windowData) context
      let focusCallback = runReaderT (focusHyprlandWindow windowData) context >>
                          return True
      item <- Gtk.menuItemNewWithLabel labelText
      _ <- Gtk.onWidgetButtonPressEvent item $ const focusCallback
      Gtk.menuShellAppend menu item
      Gtk.widgetShow item

focusHyprlandWindow :: HyprlandWindow -> TaffyIO ()
focusHyprlandWindow windowData = do
  result <- runCommand "hyprctl" ["dispatch", "focuswindow", "address:" <> T.unpack (windowAddress windowData)]
  case result of
    Left err ->
      logPrintF "System.Taffybar.Widget.HyprlandWindows" WARNING
        "Failed to focus window: %s" err
    Right _ -> return ()
