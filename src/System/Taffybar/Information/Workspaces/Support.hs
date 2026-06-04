-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.Workspaces.Support
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Shared workspace/window helpers used by multiple widgets.
module System.Taffybar.Information.Workspaces.Support
  ( WindowIconPixbufGetter,
    sortWindowsByPosition,
    sortWindowsByStackIndex,
    scaledWindowIconPixbufGetter,
    constantScaleWindowIconPixbufGetter,
    handleIconGetterException,
    getWindowIconPixbufFromClassHints,
    getWindowIconPixbufFromDesktopEntry,
    getWindowIconPixbufFromClass,
    getWindowIconPixbufByClassHints,
    getWindowIconPixbufFromEWMH,
    defaultGetWindowIconPixbuf,
    unscaledDefaultGetWindowIconPixbuf,
    addCustomIconsToDefaultWithFallbackByPath,
    addCustomIconsAndFallback,
    defaultOnWorkspaceClick,
    defaultOnWorkspaceClickEWMH,
    defaultOnWindowClick,
  )
where

import Control.Exception.Enclosed (catchAny)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Int (Int32)
import Data.List (elemIndex, sortBy, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import System.Log.Logger (Priority (..), logM)
import System.Taffybar.Context (Backend (..), TaffyIO, backend, runX11Def)
import System.Taffybar.Hyprland (getHyprlandClient)
import System.Taffybar.Information.EWMHDesktopInfo
  ( WorkspaceId (WorkspaceId),
    focusWindow,
    getWindowsStacking,
    switchToWorkspace,
  )
import qualified System.Taffybar.Information.Hyprland.API as HyprAPI
import System.Taffybar.Information.Workspaces.Model
import System.Taffybar.Util (getPixbufFromFilePath, (<|||>))
import System.Taffybar.Widget.Util
  ( handlePixbufGetterException,
    scaledPixbufGetter,
  )
import System.Taffybar.WindowIcon
  ( getCachedIconPixBufFromEWMH,
    getCachedWindowIconFromClasses,
    getCachedWindowIconFromDesktopEntryByClasses,
  )

type WindowIconPixbufGetter = Int32 -> WindowInfo -> TaffyIO (Maybe Gdk.Pixbuf)

sortWindowsByPosition :: [WindowInfo] -> [WindowInfo]
sortWindowsByPosition =
  sortOn $ \windowInfo ->
    ( windowMinimized windowInfo,
      fromMaybe (999999999, 999999999) (windowPosition windowInfo)
    )

sortWindowsByStackIndex :: [WindowInfo] -> TaffyIO [WindowInfo]
sortWindowsByStackIndex windows = do
  stackingWindows <- runX11Def [] getWindowsStacking
  let getStackIdx windowInfo =
        case windowIdentity windowInfo of
          X11WindowIdentity wid -> fromMaybe (-1) $ elemIndex (fromIntegral wid) stackingWindows
          HyprlandWindowIdentity _ -> -1
      compareWindowData a b = compare (getStackIdx b) (getStackIdx a)
  pure $ sortBy compareWindowData windows

scaledWindowIconPixbufGetter :: WindowIconPixbufGetter -> WindowIconPixbufGetter
scaledWindowIconPixbufGetter = scaledPixbufGetter

constantScaleWindowIconPixbufGetter ::
  Int32 -> WindowIconPixbufGetter -> WindowIconPixbufGetter
constantScaleWindowIconPixbufGetter constantSize getter =
  const $ scaledWindowIconPixbufGetter getter constantSize

handleIconGetterException :: WindowIconPixbufGetter -> WindowIconPixbufGetter
handleIconGetterException = handlePixbufGetterException wLog

getWindowIconPixbufFromClassHints :: WindowIconPixbufGetter
getWindowIconPixbufFromClassHints =
  getWindowIconPixbufFromDesktopEntry <|||> getWindowIconPixbufFromClass

getWindowIconPixbufFromDesktopEntry :: WindowIconPixbufGetter
getWindowIconPixbufFromDesktopEntry = handleIconGetterException $ \size winInfo ->
  tryHints size (map T.unpack (windowClassHints winInfo))
  where
    tryHints _ [] = pure Nothing
    tryHints requestedSize (klass : rest) = do
      fromDesktopEntry <- getCachedWindowIconFromDesktopEntryByClasses requestedSize klass
      case fromDesktopEntry of
        Just _ -> pure fromDesktopEntry
        Nothing -> tryHints requestedSize rest

getWindowIconPixbufFromClass :: WindowIconPixbufGetter
getWindowIconPixbufFromClass = handleIconGetterException $ \size winInfo ->
  tryHints size (map T.unpack (windowClassHints winInfo))
  where
    tryHints _ [] = pure Nothing
    tryHints requestedSize (klass : rest) = do
      fromClass <- getCachedWindowIconFromClasses requestedSize klass
      case fromClass of
        Just _ -> pure fromClass
        Nothing -> tryHints requestedSize rest

getWindowIconPixbufByClassHints :: WindowIconPixbufGetter
getWindowIconPixbufByClassHints = getWindowIconPixbufFromClassHints

getWindowIconPixbufFromEWMH :: WindowIconPixbufGetter
getWindowIconPixbufFromEWMH = handleIconGetterException $ \size windowData ->
  case windowIdentity windowData of
    X11WindowIdentity wid -> getCachedIconPixBufFromEWMH size (fromIntegral wid)
    HyprlandWindowIdentity _ -> pure Nothing

defaultGetWindowIconPixbuf :: WindowIconPixbufGetter
defaultGetWindowIconPixbuf =
  scaledWindowIconPixbufGetter unscaledDefaultGetWindowIconPixbuf

unscaledDefaultGetWindowIconPixbuf :: WindowIconPixbufGetter
unscaledDefaultGetWindowIconPixbuf =
  getWindowIconPixbufFromDesktopEntry
    <|||> getWindowIconPixbufFromClass
    <|||> getWindowIconPixbufFromEWMH

addCustomIconsToDefaultWithFallbackByPath ::
  (WindowInfo -> Maybe FilePath) ->
  FilePath ->
  WindowIconPixbufGetter
addCustomIconsToDefaultWithFallbackByPath getCustomIconPath fallbackPath =
  addCustomIconsAndFallback
    getCustomIconPath
    (const $ liftIO $ getPixbufFromFilePath fallbackPath)
    unscaledDefaultGetWindowIconPixbuf

addCustomIconsAndFallback ::
  (WindowInfo -> Maybe FilePath) ->
  (Int32 -> TaffyIO (Maybe Gdk.Pixbuf)) ->
  WindowIconPixbufGetter ->
  WindowIconPixbufGetter
addCustomIconsAndFallback getCustomIconPath fallback defaultGetter =
  scaledWindowIconPixbufGetter $
    getCustomIcon <|||> defaultGetter <|||> (\size _ -> fallback size)
  where
    getCustomIcon :: WindowIconPixbufGetter
    getCustomIcon _ windowInfo =
      maybe (pure Nothing) (liftIO . getPixbufFromFilePath) $
        getCustomIconPath windowInfo

defaultOnWorkspaceClick :: WorkspaceInfo -> TaffyIO ()
defaultOnWorkspaceClick workspaceInfo = do
  backendType <- asks backend
  case backendType of
    BackendX11 -> defaultOnWorkspaceClickEWMH workspaceInfo
    BackendWayland -> defaultOnWorkspaceClickHyprland workspaceInfo

defaultOnWorkspaceClickHyprland :: WorkspaceInfo -> TaffyIO ()
defaultOnWorkspaceClickHyprland workspaceInfo = do
  client <- getHyprlandClient
  let targetText = workspaceName (workspaceIdentity workspaceInfo)
  case HyprAPI.mkHyprlandWorkspaceTarget targetText of
    Left err ->
      wLog WARNING $
        "Failed to build Hyprland workspace target for " <> show targetText <> ": " <> show err
    Right target -> do
      result <- liftIO $ HyprAPI.dispatchHyprland client (HyprAPI.DispatchWorkspace target)
      case result of
        Left err ->
          wLog WARNING $
            "Failed to switch workspace via Hyprland dispatch: " <> show err
        Right _ -> pure ()

defaultOnWorkspaceClickEWMH :: WorkspaceInfo -> TaffyIO ()
defaultOnWorkspaceClickEWMH workspaceInfo =
  case workspaceNumericId (workspaceIdentity workspaceInfo) of
    Nothing ->
      wLog WARNING $
        "Workspace has no numeric id for EWMH switch: " <> show (workspaceIdentity workspaceInfo)
    Just workspaceId ->
      runX11Def () (switchToWorkspace (WorkspaceId workspaceId))
        `catchAny` \err ->
          wLog WARNING $
            "Failed to switch EWMH workspace " <> show workspaceId <> ": " <> show err

defaultOnWindowClick :: WindowInfo -> TaffyIO ()
defaultOnWindowClick windowInfo =
  case windowIdentity windowInfo of
    X11WindowIdentity wid ->
      runX11Def () (focusWindow (fromIntegral wid))
        `catchAny` \err ->
          wLog WARNING $
            "Failed to focus X11 window " <> show wid <> ": " <> show err
    HyprlandWindowIdentity address -> do
      client <- getHyprlandClient
      case HyprAPI.mkHyprlandAddress address of
        Left err ->
          wLog WARNING $
            "Failed to build Hyprland window address " <> show address <> ": " <> show err
        Right hyprlandAddress -> do
          result <-
            liftIO $
              HyprAPI.dispatchHyprland client (HyprAPI.DispatchFocusWindowAddress hyprlandAddress)
          case result of
            Left err ->
              wLog WARNING $
                "Failed to focus Hyprland window " <> show address <> ": " <> show err
            Right _ -> pure ()

wLog :: Priority -> String -> TaffyIO ()
wLog level message =
  liftIO $ logM "System.Taffybar.Information.Workspaces.Support" level message
