{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Workspaces
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Channel-driven workspace widget that renders backend-agnostic workspace state.
module System.Taffybar.Widget.Workspaces
  ( WorkspacesConfig (..),
    WorkspaceWidgetController (..),
    ControllerConstructor,
    defaultWidgetBuilder,
    WindowIconPixbufGetter,
    defaultWorkspacesConfig,
    defaultEWMHWorkspacesConfig,
    workspacesNew,
    hideEmpty,
    sortWindowsByPosition,
    sortWindowsByStackIndex,
    scaledWindowIconPixbufGetter,
    constantScaleWindowIconPixbufGetter,
    handleIconGetterException,
    getWindowIconPixbufFromClassHints,
    getWindowIconPixbufFromDesktopEntry,
    getWindowIconPixbufFromClass,
    getWindowIconPixbufByClassHints,
    getWindowIconPixbufFromChrome,
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

import qualified Control.Concurrent.MVar as MV
import Control.Concurrent.STM.TChan (TChan)
import Control.Exception.Enclosed (catchAny)
import Control.Monad (foldM, forM_, guard, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, asks, runReaderT)
import Data.Default (Default (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (elemIndex, sortBy, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word64)
import qualified GI.Gdk.Enums as Gdk
import qualified GI.Gdk.Structs.EventScroll as Gdk
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import System.Log.Logger (Priority (..), logM)
import System.Taffybar.Context (Backend (..), TaffyIO, backend, runX11Def)
import System.Taffybar.Hyprland (getHyprlandClient)
import System.Taffybar.Information.EWMHDesktopInfo
  ( WorkspaceId (WorkspaceId),
    ewmhWMIcon,
    focusWindow,
    getWindowsStacking,
    switchToWorkspace,
  )
import qualified System.Taffybar.Information.Hyprland.API as HyprAPI
import System.Taffybar.Information.Workspaces.EWMH
  ( EWMHWorkspaceProviderConfig,
    defaultEWMHWorkspaceProviderConfig,
    getEWMHWorkspaceStateChanAndVarWith,
    workspaceUpdateEvents,
  )
import System.Taffybar.Information.Workspaces.Hyprland
  ( getHyprlandWorkspaceStateChanAndVar,
  )
import System.Taffybar.Information.Workspaces.Model
import System.Taffybar.Util (getPixbufFromFilePath, postGUIASync, (<|||>))
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)
import System.Taffybar.Widget.Generic.ScalingImage (getScalingImageStrategy)
import System.Taffybar.Widget.Util
  ( WindowIconWidget (..),
    computeIconStripLayout,
    handlePixbufGetterException,
    scaledPixbufGetter,
    syncWidgetPool,
    updateWindowIconWidgetState,
    widgetSetClassGI,
    windowStatusClassFromFlags,
  )
import System.Taffybar.Widget.Workspaces.Shared
  ( WorkspaceState (..),
    buildWorkspaceIconLabelOverlay,
    mkWorkspaceIconWidget,
    setWorkspaceWidgetStatusClass,
  )
import System.Taffybar.WindowIcon
  ( getCachedIconPixBufFromEWMH,
    getCachedWindowIconFromClasses,
    getCachedWindowIconFromDesktopEntryByClasses,
    getPixBufFromChromeData,
    pixBufFromColor,
  )

type WindowIconPixbufGetter = Int32 -> WindowInfo -> TaffyIO (Maybe Gdk.Pixbuf)

data WorkspaceWidgetController = WorkspaceWidgetController
  { controllerWidget :: Gtk.Widget,
    controllerUpdate :: WorkspaceInfo -> TaffyIO (),
    controllerRefreshIcons :: WorkspaceInfo -> TaffyIO ()
  }

type ControllerConstructor =
  WorkspacesConfig -> WorkspaceInfo -> TaffyIO WorkspaceWidgetController

data WorkspacesConfig = WorkspacesConfig
  { widgetGap :: Int,
    widgetBuilder :: ControllerConstructor,
    iconSize :: Maybe Int32,
    maxIcons :: Maybe Int,
    minIcons :: Int,
    getWindowIconPixbuf :: WindowIconPixbufGetter,
    labelSetter :: WorkspaceInfo -> TaffyIO String,
    showWorkspaceFn :: WorkspaceInfo -> Bool,
    iconSort :: [WindowInfo] -> TaffyIO [WindowInfo],
    urgentWorkspaceState :: Bool,
    onWorkspaceClick :: WorkspaceInfo -> TaffyIO (),
    onWindowClick :: WindowInfo -> TaffyIO ()
  }

data WorkspaceEntry = WorkspaceEntry
  { entryWrapper :: Gtk.Widget,
    entryButton :: Gtk.EventBox,
    entryController :: WorkspaceWidgetController,
    entryWorkspaceRef :: IORef WorkspaceInfo,
    entryLastWorkspace :: WorkspaceInfo
  }

data WorkspaceCache = WorkspaceCache
  { cacheEntries :: M.Map WorkspaceIdentity WorkspaceEntry,
    cacheOrder :: [WorkspaceIdentity],
    cacheLastWorkspaces :: [WorkspaceInfo],
    cacheLastRevision :: Word64
  }

defaultChannelEWMHWorkspaceProviderConfig :: EWMHWorkspaceProviderConfig
defaultChannelEWMHWorkspaceProviderConfig =
  defaultEWMHWorkspaceProviderConfig
    { workspaceUpdateEvents =
        ewmhWMIcon : workspaceUpdateEvents defaultEWMHWorkspaceProviderConfig
    }

defaultEWMHWorkspaceStateSource ::
  TaffyIO (TChan WorkspaceSnapshot, MV.MVar WorkspaceSnapshot)
defaultEWMHWorkspaceStateSource =
  getEWMHWorkspaceStateChanAndVarWith defaultChannelEWMHWorkspaceProviderConfig

autoWorkspaceStateSource :: TaffyIO (TChan WorkspaceSnapshot, MV.MVar WorkspaceSnapshot)
autoWorkspaceStateSource = do
  backendType <- asks backend
  case backendType of
    BackendWayland -> getHyprlandWorkspaceStateChanAndVar
    BackendX11 -> defaultEWMHWorkspaceStateSource

defaultWorkspacesConfig :: WorkspacesConfig
defaultWorkspacesConfig =
  WorkspacesConfig
    { widgetGap = 0,
      widgetBuilder = defaultWidgetBuilder,
      iconSize = Just 16,
      maxIcons = Nothing,
      minIcons = 0,
      getWindowIconPixbuf = defaultGetWindowIconPixbuf,
      labelSetter = return . T.unpack . workspaceName . workspaceIdentity,
      showWorkspaceFn = \ws -> hideEmpty ws && not (workspaceIsSpecial ws),
      iconSort = pure . sortWindowsByPosition,
      urgentWorkspaceState = False,
      onWorkspaceClick = defaultOnWorkspaceClick,
      onWindowClick = defaultOnWindowClick
    }

defaultEWMHWorkspacesConfig :: WorkspacesConfig
defaultEWMHWorkspacesConfig = defaultWorkspacesConfig

defaultWidgetBuilder :: ControllerConstructor
defaultWidgetBuilder cfg wsInfo = do
  wsRef <- liftIO $ newIORef wsInfo
  lastWorkspaceRef <- liftIO $ newIORef wsInfo
  iconsRef <- liftIO $ newIORef []
  iconsBox <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0
  label <- liftIO $ Gtk.labelNew Nothing
  _ <- widgetSetClassGI label "workspace-label"
  iconsWidget <- Gtk.toWidget iconsBox
  labelWidget <- Gtk.toWidget label
  contents <- buildWorkspaceIconLabelOverlay iconsWidget labelWidget
  let updateController forceIcons newWs = do
        oldWs <- liftIO $ readIORef lastWorkspaceRef
        liftIO $ writeIORef wsRef newWs
        labelText <- labelSetter cfg newWs
        let wsState = toCSSState cfg newWs
        liftIO $ Gtk.labelSetMarkup label (T.pack labelText)
        liftIO $ setWorkspaceWidgetStatusClass wsState contents
        liftIO $ setWorkspaceWidgetStatusClass wsState label
        currentIcons <- liftIO $ readIORef iconsRef
        let needsIconUpdate =
              forceIcons
                || null currentIcons
                || workspaceWindows oldWs /= workspaceWindows newWs
        updatedIcons <-
          if needsIconUpdate
            then updateWorkspaceIcons cfg wsRef iconsBox currentIcons newWs
            else return currentIcons
        liftIO $ writeIORef iconsRef updatedIcons
        liftIO $ writeIORef lastWorkspaceRef newWs
      controller =
        WorkspaceWidgetController
          { controllerWidget = contents,
            controllerUpdate = updateController False,
            controllerRefreshIcons = updateController True
          }
  controllerRefreshIcons controller wsInfo
  return controller

instance Default WorkspacesConfig where
  def = defaultWorkspacesConfig

hideEmpty :: WorkspaceInfo -> Bool
hideEmpty WorkspaceInfo {workspaceState = WorkspaceEmpty} = False
hideEmpty _ = True

sortWindowsByPosition :: [WindowInfo] -> [WindowInfo]
sortWindowsByPosition =
  sortOn $ \w ->
    ( windowMinimized w,
      fromMaybe (999999999, 999999999) (windowPosition w)
    )

sortWindowsByStackIndex :: [WindowInfo] -> TaffyIO [WindowInfo]
sortWindowsByStackIndex wins = do
  stackingWindows <- runX11Def [] getWindowsStacking
  let getStackIdx windowInfo =
        case windowIdentity windowInfo of
          X11WindowIdentity wid -> fromMaybe (-1) $ elemIndex (fromIntegral wid) stackingWindows
          HyprlandWindowIdentity _ -> -1
      compareWindowData a b = compare (getStackIdx b) (getStackIdx a)
  return $ sortBy compareWindowData wins

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
    tryHints _ [] = return Nothing
    tryHints requestedSize (klass : rest) = do
      fromDesktopEntry <- getCachedWindowIconFromDesktopEntryByClasses requestedSize klass
      case fromDesktopEntry of
        Just _ -> return fromDesktopEntry
        Nothing -> tryHints requestedSize rest

getWindowIconPixbufFromClass :: WindowIconPixbufGetter
getWindowIconPixbufFromClass = handleIconGetterException $ \size winInfo ->
  tryHints size (map T.unpack (windowClassHints winInfo))
  where
    tryHints _ [] = return Nothing
    tryHints requestedSize (klass : rest) = do
      fromClass <- getCachedWindowIconFromClasses requestedSize klass
      case fromClass of
        Just _ -> return fromClass
        Nothing -> tryHints requestedSize rest

getWindowIconPixbufByClassHints :: Int32 -> WindowInfo -> TaffyIO (Maybe Gdk.Pixbuf)
getWindowIconPixbufByClassHints = getWindowIconPixbufFromClassHints

getWindowIconPixbufFromChrome :: WindowIconPixbufGetter
getWindowIconPixbufFromChrome _ windowData =
  case windowIdentity windowData of
    X11WindowIdentity wid -> getPixBufFromChromeData (fromIntegral wid)
    HyprlandWindowIdentity _ -> return Nothing

getWindowIconPixbufFromEWMH :: WindowIconPixbufGetter
getWindowIconPixbufFromEWMH = handleIconGetterException $ \size windowData ->
  case windowIdentity windowData of
    X11WindowIdentity wid -> getCachedIconPixBufFromEWMH size (fromIntegral wid)
    HyprlandWindowIdentity _ -> return Nothing

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
    getCustomIcon <|||> defaultGetter <|||> (\s _ -> fallback s)
  where
    getCustomIcon :: WindowIconPixbufGetter
    getCustomIcon _ windowInfo =
      maybe (return Nothing) (liftIO . getPixbufFromFilePath) $
        getCustomIconPath windowInfo

defaultOnWorkspaceClick :: WorkspaceInfo -> TaffyIO ()
defaultOnWorkspaceClick wsInfo = do
  backendType <- asks backend
  case backendType of
    BackendX11 -> defaultOnWorkspaceClickEWMH wsInfo
    BackendWayland -> defaultOnWorkspaceClickHyprland wsInfo

defaultOnWorkspaceClickHyprland :: WorkspaceInfo -> TaffyIO ()
defaultOnWorkspaceClickHyprland wsInfo = do
  client <- getHyprlandClient
  let targetText = workspaceName (workspaceIdentity wsInfo)
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
        Right _ -> return ()

defaultOnWorkspaceClickEWMH :: WorkspaceInfo -> TaffyIO ()
defaultOnWorkspaceClickEWMH wsInfo =
  case workspaceNumericId (workspaceIdentity wsInfo) of
    Nothing ->
      wLog WARNING $
        "Workspace has no numeric id for EWMH switch: " <> show (workspaceIdentity wsInfo)
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
        Right addr -> do
          result <-
            liftIO $
              HyprAPI.dispatchHyprland client (HyprAPI.DispatchFocusWindowAddress addr)
          case result of
            Left err ->
              wLog WARNING $
                "Failed to focus Hyprland window " <> show address <> ": " <> show err
            Right _ -> return ()

workspacesNew :: WorkspacesConfig -> TaffyIO Gtk.Widget
workspacesNew cfg = do
  ctx <- ask
  cont <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal (fromIntegral $ widgetGap cfg)
  _ <- widgetSetClassGI cont "workspaces"
  cacheVar <- liftIO $ MV.newMVar (WorkspaceCache M.empty [] [] 0)
  (chan, snapshotVar) <- autoWorkspaceStateSource
  initialSnapshot <- liftIO $ MV.readMVar snapshotVar
  renderWorkspaces cfg cont cacheVar initialSnapshot
  _ <-
    liftIO $
      Gtk.onWidgetRealize cont $ do
        latestSnapshot <- MV.readMVar snapshotVar
        postGUIASync $
          runReaderT
            (renderWorkspaces cfg cont cacheVar latestSnapshot)
            ctx
  _ <-
    liftIO $
      channelWidgetNew
        cont
        chan
        (\snapshot -> postGUIASync $ runReaderT (renderWorkspaces cfg cont cacheVar snapshot) ctx)
  Gtk.toWidget cont

renderWorkspaces ::
  WorkspacesConfig ->
  Gtk.Box ->
  MV.MVar WorkspaceCache ->
  WorkspaceSnapshot ->
  TaffyIO ()
renderWorkspaces cfg cont cacheVar snapshot = do
  ctx <- ask
  liftIO $
    MV.modifyMVar_ cacheVar $ \oldCache ->
      runReaderT (updateCache cfg cont cacheVar snapshot oldCache) ctx

updateCache ::
  WorkspacesConfig ->
  Gtk.Box ->
  MV.MVar WorkspaceCache ->
  WorkspaceSnapshot ->
  WorkspaceCache ->
  TaffyIO WorkspaceCache
updateCache cfg cont cacheVar snapshot oldCache = do
  let workspaces = snapshotWorkspaces snapshot
      forceIconRefresh =
        snapshotBackend snapshot == WorkspaceBackendEWMH
          && snapshotRevision snapshot /= cacheLastRevision oldCache
  if workspaces == cacheLastWorkspaces oldCache && not forceIconRefresh
    then return oldCache
    else do
      let oldEntries = cacheEntries oldCache
          buildOrUpdate (cacheAcc, entriesAcc) wsInfo = do
            let wsKey = workspaceIdentity wsInfo
            entry <-
              case M.lookup wsKey oldEntries of
                Just existing
                  | entryLastWorkspace existing == wsInfo ->
                      if forceIconRefresh
                        then refreshWorkspaceEntryIcons existing wsInfo
                        else return existing
                  | otherwise -> updateWorkspaceEntry existing wsInfo
                Nothing -> buildWorkspaceEntry cfg cacheVar wsInfo
            return (M.insert wsKey entry cacheAcc, (wsInfo, entry) : entriesAcc)
      (newEntries, orderedEntriesRev) <-
        foldM buildOrUpdate (M.empty, []) workspaces
      let orderedEntries = reverse orderedEntriesRev
          removed = M.difference oldEntries newEntries
          added = M.difference newEntries oldEntries
      forM_ (M.elems removed) $
        liftIO . Gtk.containerRemove cont . entryWrapper
      forM_ (M.elems added) $ \entry -> do
        liftIO $ Gtk.containerAdd cont (entryWrapper entry)
        liftIO $ Gtk.widgetShowAll (entryWrapper entry)
      let desiredOrder = map (workspaceIdentity . fst) orderedEntries
          needsReorder =
            desiredOrder /= cacheOrder oldCache
              || not (M.null added)
              || not (M.null removed)
      when needsReorder $
        forM_ (zip [0 :: Int ..] orderedEntries) $ \(position, (_wsInfo, entry)) ->
          liftIO $ Gtk.boxReorderChild cont (entryWrapper entry) (fromIntegral position)
      forM_ orderedEntries $ \(wsInfo, entry) ->
        if showWorkspaceFn cfg wsInfo
          then liftIO $ Gtk.widgetShow (entryButton entry)
          else liftIO $ Gtk.widgetHide (entryButton entry)
      return
        WorkspaceCache
          { cacheEntries = newEntries,
            cacheOrder = desiredOrder,
            cacheLastWorkspaces = workspaces,
            cacheLastRevision = snapshotRevision snapshot
          }

buildWorkspaceEntry ::
  WorkspacesConfig ->
  MV.MVar WorkspaceCache ->
  WorkspaceInfo ->
  TaffyIO WorkspaceEntry
buildWorkspaceEntry cfg cacheVar wsInfo = do
  wsRef <- liftIO $ newIORef wsInfo
  controller <- widgetBuilder cfg cfg wsInfo
  button <- liftIO Gtk.eventBoxNew
  _ <- widgetSetClassGI button "workspace-button"
  liftIO $ Gtk.eventBoxSetVisibleWindow button False
  liftIO $ Gtk.containerAdd button (controllerWidget controller)
  wrapperBox <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0
  liftIO $ Gtk.containerAdd wrapperBox button
  wrapper <- Gtk.toWidget wrapperBox
  _ <- widgetSetClassGI wrapper "workspace-wrapper"
  ctx <- ask
  _ <-
    liftIO $
      Gtk.onWidgetButtonPressEvent button $
        const $ do
          currentWs <- readIORef wsRef
          runReaderT (onWorkspaceClick cfg currentWs) ctx
          return True
  _ <-
    liftIO $
      Gtk.onWidgetScrollEvent button $ \scrollEvent -> do
        direction <- Gdk.getEventScrollDirection scrollEvent
        let scrollPrevious = runReaderT (switchWorkspaceRelative cfg cacheVar wsRef True) ctx
            scrollNext = runReaderT (switchWorkspaceRelative cfg cacheVar wsRef False) ctx
        case direction of
          Gdk.ScrollDirectionUp -> scrollPrevious
          Gdk.ScrollDirectionLeft -> scrollPrevious
          Gdk.ScrollDirectionDown -> scrollNext
          Gdk.ScrollDirectionRight -> scrollNext
          _ -> return False
  return
    WorkspaceEntry
      { entryWrapper = wrapper,
        entryButton = button,
        entryController = controller,
        entryWorkspaceRef = wsRef,
        entryLastWorkspace = wsInfo
      }

updateWorkspaceEntry ::
  WorkspaceEntry ->
  WorkspaceInfo ->
  TaffyIO WorkspaceEntry
updateWorkspaceEntry entry wsInfo = do
  liftIO $ writeIORef (entryWorkspaceRef entry) wsInfo
  controllerUpdate (entryController entry) wsInfo
  return entry {entryLastWorkspace = wsInfo}

refreshWorkspaceEntryIcons ::
  WorkspaceEntry ->
  WorkspaceInfo ->
  TaffyIO WorkspaceEntry
refreshWorkspaceEntryIcons entry wsInfo = do
  liftIO $ writeIORef (entryWorkspaceRef entry) wsInfo
  controllerRefreshIcons (entryController entry) wsInfo
  return entry {entryLastWorkspace = wsInfo}

switchWorkspaceRelative ::
  WorkspacesConfig ->
  MV.MVar WorkspaceCache ->
  IORef WorkspaceInfo ->
  Bool ->
  TaffyIO Bool
switchWorkspaceRelative cfg cacheVar wsRef moveBackward = do
  currentWs <- liftIO $ readIORef wsRef
  cache <- liftIO $ MV.readMVar cacheVar
  let identities = cacheOrder cache
      currentIdentity = workspaceIdentity currentWs
      step = if moveBackward then (-1) else 1
      getTargetIdentity = do
        idx <- elemIndex currentIdentity identities
        let total = length identities
        guard (total > 0)
        let wrappedIndex = (idx + step + total) `mod` total
        return (identities !! wrappedIndex)
  case getTargetIdentity >>= (`M.lookup` cacheEntries cache) of
    Nothing -> return False
    Just targetEntry -> do
      targetWorkspace <- liftIO $ readIORef (entryWorkspaceRef targetEntry)
      onWorkspaceClick cfg targetWorkspace
      return True

updateWorkspaceIcons ::
  WorkspacesConfig ->
  IORef WorkspaceInfo ->
  Gtk.Box ->
  [WindowIconWidget WindowInfo] ->
  WorkspaceInfo ->
  TaffyIO [WindowIconWidget WindowInfo]
updateWorkspaceIcons cfg workspaceRef iconsBox iconWidgets wsInfo = do
  sortedWindows <- iconSort cfg (workspaceWindows wsInfo)
  let (effectiveMinIcons, _targetLen, paddedWindows) =
        computeIconStripLayout
          (minIcons cfg)
          (maxIcons cfg)
          sortedWindows
      buildOne i = buildIconWidget cfg workspaceRef (i < effectiveMinIcons)
      updateOne = updateIconWidget
  syncWidgetPool iconsBox iconWidgets paddedWindows buildOne iconContainer updateOne

buildIconWidget ::
  WorkspacesConfig ->
  IORef WorkspaceInfo ->
  Bool ->
  TaffyIO (WindowIconWidget WindowInfo)
buildIconWidget cfg workspaceRef transparentOnNone = do
  ctx <- ask
  strategy <- getScalingImageStrategy
  let getPB size windowInfo = runReaderT (getWindowIconPixbuf cfg size windowInfo) ctx
  iconWidget <-
    liftIO $
      mkWorkspaceIconWidget
        strategy
        (iconSize cfg)
        transparentOnNone
        getPB
        (`pixBufFromColor` 0)
  _ <-
    liftIO $
      Gtk.onWidgetButtonPressEvent (iconContainer iconWidget) $
        const $ do
          maybeWindow <- MV.readMVar (iconWindow iconWidget)
          case maybeWindow of
            Just windowInfo -> runReaderT (onWindowClick cfg windowInfo) ctx
            Nothing -> do
              currentWs <- readIORef workspaceRef
              runReaderT (onWorkspaceClick cfg currentWs) ctx
          return True
  return iconWidget

updateIconWidget :: WindowIconWidget WindowInfo -> Maybe WindowInfo -> TaffyIO ()
updateIconWidget iconWidget windowData =
  updateWindowIconWidgetState
    iconWidget
    windowData
    windowTitle
    getWindowStatusString

getWindowStatusString :: WindowInfo -> T.Text
getWindowStatusString windowInfo =
  windowStatusClassFromFlags
    (windowMinimized windowInfo)
    (windowActive windowInfo)
    (windowUrgent windowInfo)

toCSSState :: WorkspacesConfig -> WorkspaceInfo -> WorkspaceState
toCSSState cfg wsInfo
  | urgentWorkspaceState cfg
      && workspaceState wsInfo == WorkspaceHidden
      && workspaceHasUrgentWindow wsInfo =
      Urgent
  | otherwise =
      case workspaceState wsInfo of
        WorkspaceActive -> Active
        WorkspaceVisible -> Visible
        WorkspaceHidden -> Hidden
        WorkspaceEmpty -> Empty

wLog :: Priority -> String -> TaffyIO ()
wLog level message =
  liftIO $ logM "System.Taffybar.Widget.Workspaces" level message
