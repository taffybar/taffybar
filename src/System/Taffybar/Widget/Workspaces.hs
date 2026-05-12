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
import Control.Monad (foldM, forM_, guard, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ask, asks, runReaderT)
import Data.Default (Default (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (elemIndex)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Word (Word64)
import qualified GI.Gdk.Enums as Gdk
import qualified GI.Gdk.Structs.EventScroll as Gdk
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (Backend (..), TaffyIO, backend)
import System.Taffybar.Information.EWMHDesktopInfo
  ( ewmhWMIcon,
  )
import System.Taffybar.Information.Workspaces.EWMH
  ( EWMHWorkspaceProviderConfig,
    defaultEWMHWorkspaceProviderConfig,
    getEWMHWorkspaceStateChanAndVarWith,
    workspaceUpdateEvents,
  )
import System.Taffybar.Information.Workspaces.Hyprland
  ( HyprlandWorkspaceProviderConfig,
    defaultHyprlandWorkspaceProviderConfig,
    getHyprlandWorkspaceStateChanAndVarWith,
  )
import System.Taffybar.Information.Workspaces.Model
import System.Taffybar.Information.Workspaces.Support
  ( WindowIconPixbufGetter,
    addCustomIconsAndFallback,
    addCustomIconsToDefaultWithFallbackByPath,
    constantScaleWindowIconPixbufGetter,
    defaultGetWindowIconPixbuf,
    defaultOnWindowClick,
    defaultOnWorkspaceClick,
    defaultOnWorkspaceClickEWMH,
    getWindowIconPixbufByClassHints,
    getWindowIconPixbufFromChrome,
    getWindowIconPixbufFromClass,
    getWindowIconPixbufFromClassHints,
    getWindowIconPixbufFromDesktopEntry,
    getWindowIconPixbufFromEWMH,
    handleIconGetterException,
    scaledWindowIconPixbufGetter,
    sortWindowsByPosition,
    sortWindowsByStackIndex,
    unscaledDefaultGetWindowIconPixbuf,
  )
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.AutoSizeImage (ImageScaleStrategy)
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)
import System.Taffybar.Widget.Generic.ScalingImage
  ( getScalingImageStrategy,
    scalingImageNew,
  )
import System.Taffybar.Widget.Util
  ( WindowIconWidget (..),
    buildBottomLeftAlignedBox,
    buildContentsBox,
    buildOverlayWithPassThrough,
    computeIconStripLayout,
    mkWindowIconWidgetBase,
    syncWidgetPool,
    updateWidgetClasses,
    widgetSetClassGI,
    windowStatusClassFromFlags,
  )
import System.Taffybar.WindowIcon
  ( pixBufFromColor,
  )

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
    hyprlandWorkspaceProviderConfig :: HyprlandWorkspaceProviderConfig,
    urgentWorkspaceState :: Bool,
    onWorkspaceClick :: WorkspaceInfo -> TaffyIO (),
    onWindowClick :: WindowInfo -> TaffyIO ()
  }

data WorkspaceState
  = Active
  | Visible
  | Hidden
  | Empty
  | Urgent
  deriving (Show, Eq)

getCSSClass :: (Show s) => s -> T.Text
getCSSClass = T.toLower . T.pack . show

cssWorkspaceStates :: [T.Text]
cssWorkspaceStates = map getCSSClass [Active, Visible, Hidden, Empty, Urgent]

setWorkspaceWidgetStatusClass ::
  (MonadIO m, Gtk.IsWidget a) => WorkspaceState -> a -> m ()
setWorkspaceWidgetStatusClass ws widget =
  updateWidgetClasses
    widget
    [getCSSClass ws]
    cssWorkspaceStates

-- | Build the common overlay layout used by workspace widgets:
-- window icons are the base content and the workspace label is overlaid in the
-- bottom-left corner.
buildWorkspaceIconLabelOverlay ::
  (MonadIO m) =>
  Gtk.Widget ->
  Gtk.Widget ->
  m Gtk.Widget
buildWorkspaceIconLabelOverlay iconsWidget labelWidget = do
  base <- buildContentsBox iconsWidget
  overlayLabel <- buildBottomLeftAlignedBox "overlay-box" labelWidget
  buildOverlayWithPassThrough base [overlayLabel]

mkWorkspaceIconWidget ::
  ImageScaleStrategy ->
  Maybe Int32 ->
  Bool ->
  (Int32 -> a -> IO (Maybe Gdk.Pixbuf)) ->
  (Int32 -> IO Gdk.Pixbuf) ->
  IO (WindowIconWidget a)
mkWorkspaceIconWidget strategy mSize transparentOnNone getPixbufFor mkTransparent = do
  base <- mkWindowIconWidgetBase mSize
  let getPixbuf size = do
        mWin <- MV.readMVar (iconWindow base)
        case mWin of
          Nothing ->
            if transparentOnNone
              then Just <$> mkTransparent size
              else return Nothing
          Just w -> do
            pb <- getPixbufFor size w
            case pb of
              Just _ -> return pb
              Nothing ->
                if transparentOnNone
                  then Just <$> mkTransparent size
                  else return Nothing
  (imageWidget, refreshImage) <-
    scalingImageNew
      strategy
      getPixbuf
      Gtk.OrientationHorizontal
  _ <- widgetSetClassGI imageWidget "window-icon"
  forM_ mSize $ \s ->
    Gtk.widgetSetSizeRequest imageWidget (fromIntegral s) (fromIntegral s)
  Gtk.containerAdd (iconContainer base) imageWidget
  return base {iconImage = imageWidget, iconForceUpdate = refreshImage}

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

autoWorkspaceStateSource :: WorkspacesConfig -> TaffyIO (TChan WorkspaceSnapshot, MV.MVar WorkspaceSnapshot)
autoWorkspaceStateSource cfg = do
  backendType <- asks backend
  case backendType of
    BackendWayland -> getHyprlandWorkspaceStateChanAndVarWith (hyprlandWorkspaceProviderConfig cfg)
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
      hyprlandWorkspaceProviderConfig = defaultHyprlandWorkspaceProviderConfig,
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
            windowsChanged = workspaceWindows oldWs /= workspaceWindows newWs
        liftIO $ Gtk.labelSetMarkup label (T.pack labelText)
        setWorkspaceWidgetStatusClass wsState contents
        setWorkspaceWidgetStatusClass wsState label
        currentIcons <- liftIO $ readIORef iconsRef
        let needsIconUpdate =
              forceIcons
                || null currentIcons
                || windowsChanged
        updatedIcons <-
          if needsIconUpdate
            then updateWorkspaceIcons cfg wsRef iconsBox currentIcons newWs
            else return currentIcons
        when forceIcons $
          liftIO $
            forM_ updatedIcons iconForceUpdate
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

workspacesNew :: WorkspacesConfig -> TaffyIO Gtk.Widget
workspacesNew cfg = do
  ctx <- ask
  cont <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal (fromIntegral $ widgetGap cfg)
  _ <- widgetSetClassGI cont "workspaces"
  cacheVar <- liftIO $ MV.newMVar (WorkspaceCache M.empty [] [] 0)
  (chan, snapshotVar) <- autoWorkspaceStateSource cfg
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
      applyVisibility wsInfo entry =
        if showWorkspaceFn cfg wsInfo
          then liftIO $ Gtk.widgetShow (entryButton entry)
          else liftIO $ Gtk.widgetHide (entryButton entry)
  if workspaces == cacheLastWorkspaces oldCache && not forceIconRefresh
    then do
      forM_ (M.elems $ cacheEntries oldCache) $ \entry ->
        applyVisibility (entryLastWorkspace entry) entry
      return oldCache
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
      forM_ orderedEntries $ uncurry applyVisibility
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
        listToMaybe $ drop wrappedIndex identities
  case getTargetIdentity >>= (`M.lookup` cacheEntries cache) of
    Nothing -> return False
    Just targetEntry -> do
      targetWorkspace <- liftIO $ readIORef (entryWorkspaceRef targetEntry)
      onWorkspaceClick cfg targetWorkspace
      return True

activateWorkspaceIcon ::
  WorkspacesConfig ->
  WorkspaceInfo ->
  Maybe WindowInfo ->
  TaffyIO ()
activateWorkspaceIcon cfg currentWs maybeWindow = do
  backendType <- asks backend
  case (backendType, maybeWindow) of
    (BackendX11, Just windowInfo) -> onWindowClick cfg windowInfo
    _ -> onWorkspaceClick cfg currentWs

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
          currentWs <- readIORef workspaceRef
          runReaderT (activateWorkspaceIcon cfg currentWs maybeWindow) ctx
          return True
  return iconWidget

updateIconWidget :: WindowIconWidget WindowInfo -> Maybe WindowInfo -> TaffyIO ()
updateIconWidget iconWidget windowData = do
  oldWindowData <- liftIO $ MV.swapMVar (iconWindow iconWidget) windowData
  Gtk.widgetSetTooltipText (iconContainer iconWidget) (windowTitle <$> windowData)
  let pixbufKeyChanged =
        (windowIconPixbufKey <$> oldWindowData) /= (windowIconPixbufKey <$> windowData)
  when pixbufKeyChanged $
    liftIO $
      iconForceUpdate iconWidget
  let statusString = maybe "inactive" getWindowStatusString windowData
      stateClasses =
        case windowData of
          Just windowInfo | windowPinned windowInfo -> [statusString, "pinned"]
          _ -> [statusString]
  updateWidgetClasses
    (iconContainer iconWidget)
    stateClasses
    windowIconStatusClasses

windowIconPixbufKey :: WindowInfo -> (WindowIdentity, Word64, [T.Text], T.Text)
windowIconPixbufKey windowInfo =
  (windowIdentity windowInfo, windowUpdateRevision windowInfo, windowClassHints windowInfo, windowTitle windowInfo)

windowIconStatusClasses :: [T.Text]
windowIconStatusClasses = ["active", "urgent", "minimized", "normal", "inactive", "pinned"]

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
