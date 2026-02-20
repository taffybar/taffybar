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
-- switch focus to.
module System.Taffybar.Widget.Windows where

import qualified Control.Concurrent.MVar as MV
import Control.Concurrent.STM.TChan (TChan)
import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Default (Default (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (find)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import System.Taffybar.Context
import System.Taffybar.Information.EWMHDesktopInfo
  ( ewmhWMIcon,
  )
import System.Taffybar.Information.Workspaces.EWMH
  ( defaultEWMHWorkspaceProviderConfig,
    getEWMHWorkspaceStateChanAndVarWith,
    workspaceUpdateEvents,
  )
import System.Taffybar.Information.Workspaces.Hyprland
  ( getHyprlandWorkspaceStateChanAndVar,
  )
import System.Taffybar.Information.Workspaces.Model
import System.Taffybar.Util
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)
import System.Taffybar.Widget.Generic.ScalingImage (scalingImage)
import System.Taffybar.Widget.Util (widgetSetClassGI)
import System.Taffybar.Widget.Workspaces
  ( defaultOnWindowClick,
    getWindowIconPixbufByClassHints,
    sortWindowsByPosition,
  )

-- | Behavior configuration for the windows menu widget.
data WindowsConfig = WindowsConfig
  { -- | A monadic function used to build labels for windows in the menu.
    getMenuLabel :: WindowInfo -> TaffyIO T.Text,
    -- | Action to build the label text for the active window.
    getActiveLabel :: Maybe WindowInfo -> TaffyIO T.Text,
    -- | Optional function to retrieve a pixbuf to show next to the
    -- active-window label.
    getActiveWindowIconPixbuf :: Maybe (Int32 -> WindowInfo -> TaffyIO (Maybe Gdk.Pixbuf)),
    -- | Sort menu windows before rendering.
    menuWindowSort :: [WindowInfo] -> TaffyIO [WindowInfo],
    -- | Action when a menu item is selected.
    onMenuWindowClick :: WindowInfo -> TaffyIO ()
  }

defaultChannelEWMHWorkspaceStateSource ::
  TaffyIO (TChan WorkspaceSnapshot, MV.MVar WorkspaceSnapshot)
defaultChannelEWMHWorkspaceStateSource =
  getEWMHWorkspaceStateChanAndVarWith providerConfig
  where
    providerConfig =
      defaultEWMHWorkspaceProviderConfig
        { workspaceUpdateEvents =
            ewmhWMIcon : workspaceUpdateEvents defaultEWMHWorkspaceProviderConfig
        }

autoWindowStateSource :: TaffyIO (TChan WorkspaceSnapshot, MV.MVar WorkspaceSnapshot)
autoWindowStateSource = do
  backendType <- asks backend
  case backendType of
    BackendWayland -> getHyprlandWorkspaceStateChanAndVar
    BackendX11 -> defaultChannelEWMHWorkspaceStateSource

-- | Build a menu label from the window title.
defaultGetMenuLabel :: WindowInfo -> TaffyIO T.Text
defaultGetMenuLabel = truncatedGetMenuLabel 35

-- | Default active-window label renderer.
defaultGetActiveLabel :: Maybe WindowInfo -> TaffyIO T.Text
defaultGetActiveLabel = maybe (return "") defaultGetMenuLabel

-- | Truncate the active-window label to a maximum length.
truncatedGetActiveLabel :: Int -> Maybe WindowInfo -> TaffyIO T.Text
truncatedGetActiveLabel maxLength windowInfo =
  truncateText maxLength <$> defaultGetActiveLabel windowInfo

-- | Truncate window labels in the popup menu to a maximum length.
truncatedGetMenuLabel :: Int -> WindowInfo -> TaffyIO T.Text
truncatedGetMenuLabel maxLength windowInfo =
  return $ truncateText maxLength (windowTitle windowInfo)

-- | Default configuration used by 'windowsNew'.
defaultWindowsConfig :: WindowsConfig
defaultWindowsConfig =
  WindowsConfig
    { getMenuLabel = defaultGetMenuLabel,
      getActiveLabel = defaultGetActiveLabel,
      getActiveWindowIconPixbuf = Just getWindowIconPixbufByClassHints,
      menuWindowSort = pure . sortWindowsByPosition,
      onMenuWindowClick = defaultOnWindowClick
    }

-- | EWMH/X11 preset configuration.
defaultEWMHWindowsConfig :: WindowsConfig
defaultEWMHWindowsConfig = defaultWindowsConfig

instance Default WindowsConfig where
  def = defaultWindowsConfig

-- | Create a new channel-driven backend-agnostic windows widget.
windowsNew :: WindowsConfig -> TaffyIO Gtk.Widget
windowsNew config = do
  hbox <- lift $ Gtk.boxNew Gtk.OrientationHorizontal 0
  menu <- lift Gtk.menuNew
  (stateChan, stateVar) <- autoWindowStateSource
  initialSnapshot <- liftIO $ MV.readMVar stateVar
  activeWindowRef <- liftIO $ newIORef $ getActiveWindow initialSnapshot

  refreshIcon <- case getActiveWindowIconPixbuf config of
    Just getIcon -> do
      (rf, icon) <- buildWindowsIcon activeWindowRef getIcon
      Gtk.boxPackStart hbox icon True True 0
      pure rf
    Nothing -> pure (pure ())

  (setLabelTitle, label) <- buildWindowsLabel
  Gtk.boxPackStart hbox label True True 0

  let refreshFromSnapshot snapshot = do
        let activeWindow = getActiveWindow snapshot
        liftIO $ writeIORef activeWindowRef activeWindow
        labelText <- getActiveLabel config activeWindow
        lift $ setLabelTitle labelText
        lift refreshIcon
        lift $ clearMenu menu
        fillMenu config snapshot menu

  void $ refreshFromSnapshot initialSnapshot

  ctx <- ask
  _ <-
    liftIO $
      Gtk.onWidgetRealize hbox $ do
        latestSnapshot <- MV.readMVar stateVar
        void $ runReaderT (refreshFromSnapshot latestSnapshot) ctx
  _ <-
    liftIO $
      channelWidgetNew
        hbox
        stateChan
        (\snapshot -> postGUIASync $ runReaderT (refreshFromSnapshot snapshot) ctx)

  boxWidget <- Gtk.toWidget hbox
  button <- lift Gtk.menuButtonNew
  lift $ Gtk.containerAdd button boxWidget
  lift $ Gtk.menuButtonSetPopup button $ Just menu
  lift $ Gtk.widgetShowAll button
  menuButtonWidget <- Gtk.toWidget button

  widgetSetClassGI menuButtonWidget "windows"

-- | Build the active-window label and return an update action for it.
buildWindowsLabel :: TaffyIO (T.Text -> IO (), Gtk.Widget)
buildWindowsLabel = do
  label <- lift $ Gtk.labelNew Nothing
  lift $ Gtk.labelSetSingleLineMode label True
  lift $ Gtk.labelSetEllipsize label Pango.EllipsizeModeEnd
  let setLabelTitle title = postGUIASync $ Gtk.labelSetText label title
  (setLabelTitle,) <$> Gtk.toWidget label

-- | Build the active-window icon and return an update action for it.
buildWindowsIcon ::
  IORef (Maybe WindowInfo) ->
  (Int32 -> WindowInfo -> TaffyIO (Maybe Gdk.Pixbuf)) ->
  TaffyIO (IO (), Gtk.Widget)
buildWindowsIcon activeWindowRef windowIconPixbufGetter = do
  runTaffy <- asks (flip runReaderT)
  let getActiveWindowPixbuf size = runTaffy . runMaybeT $ do
        windowInfo <- MaybeT $ liftIO $ readIORef activeWindowRef
        MaybeT $ windowIconPixbufGetter size windowInfo

  (imageWidget, updateImage) <- scalingImage getActiveWindowPixbuf Gtk.OrientationHorizontal
  return (postGUIASync updateImage, imageWidget)

-- | Populate the given menu widget with the list of currently open windows.
fillMenu ::
  (Gtk.IsMenuShell a) =>
  WindowsConfig ->
  WorkspaceSnapshot ->
  a ->
  ReaderT Context IO ()
fillMenu config snapshot menu =
  ask >>= \context -> do
    windows <- menuWindowSort config (getWindows snapshot)
    forM_ windows $ \windowInfo ->
      lift $ do
        labelText <- runReaderT (getMenuLabel config windowInfo) context
        iconPixbuf <-
          case getActiveWindowIconPixbuf config of
            Nothing -> pure Nothing
            Just getIcon -> runReaderT (getIcon windowsMenuIconSize windowInfo) context
        let focusCallback =
              runReaderT (onMenuWindowClick config windowInfo) context
                >> return True
        item <- Gtk.menuItemNew
        content <- Gtk.boxNew Gtk.OrientationHorizontal 6
        _ <-
          forM iconPixbuf $ \pixbuf -> do
            icon <- Gtk.imageNewFromPixbuf (Just pixbuf)
            Gtk.boxPackStart content icon False False 0
            pure icon
        label <- Gtk.labelNew (Just labelText)
        Gtk.labelSetXalign label 0
        Gtk.boxPackStart content label True True 0
        Gtk.containerAdd item content
        _ <- Gtk.onWidgetButtonPressEvent item $ const focusCallback
        Gtk.menuShellAppend menu item
        Gtk.widgetShowAll item

clearMenu :: (Gtk.IsContainer a) => a -> IO ()
clearMenu menu =
  Gtk.containerForeach menu $ \item ->
    Gtk.containerRemove menu item >> Gtk.widgetDestroy item

windowsMenuIconSize :: Int32
windowsMenuIconSize = fromIntegral $ fromEnum Gtk.IconSizeMenu

getWindows :: WorkspaceSnapshot -> [WindowInfo]
getWindows snapshot = reverse ordered
  where
    allWindows =
      [ win
      | wsInfo <- snapshotWorkspaces snapshot,
        win <- workspaceWindows wsInfo
      ]
    (_, ordered) = foldl keepFirst (Set.empty, []) allWindows
    keepFirst (seen, acc) windowInfo
      | windowIdentity windowInfo `Set.member` seen = (seen, acc)
      | otherwise =
          (Set.insert (windowIdentity windowInfo) seen, windowInfo : acc)

getActiveWindow :: WorkspaceSnapshot -> Maybe WindowInfo
getActiveWindow snapshot =
  find windowActive allWindows
  where
    allWindows =
      [ win
      | wsInfo <- snapshotWorkspaces snapshot,
        win <- workspaceWindows wsInfo
      ]
