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
    defaultWorkspacesConfig,
    defaultEWMHWorkspacesConfig,
    workspacesNew,
    sortWindowsByPosition,
    getWindowIconPixbufByClassHints,
    defaultOnWorkspaceClick,
    defaultOnWorkspaceClickEWMH,
    defaultOnWindowClick,
  )
where

import qualified Control.Concurrent.MVar as MV
import Control.Concurrent.STM.TChan (TChan)
import Control.Exception.Enclosed (catchAny)
import Control.Monad (foldM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Default (Default (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import System.Log.Logger (Priority (..), logM)
import System.Taffybar.Context (TaffyIO, runX11Def)
import System.Taffybar.Hyprland (getHyprlandClient)
import System.Taffybar.Information.EWMHDesktopInfo
  ( WorkspaceId (WorkspaceId),
    focusWindow,
    switchToWorkspace,
  )
import qualified System.Taffybar.Information.Hyprland.API as HyprAPI
import System.Taffybar.Information.Workspaces.EWMH
  ( getEWMHWorkspaceStateChanAndVar,
  )
import System.Taffybar.Information.Workspaces.Hyprland
  ( getHyprlandWorkspaceStateChanAndVar,
  )
import System.Taffybar.Information.Workspaces.Model
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)
import System.Taffybar.Widget.Generic.ScalingImage (getScalingImageStrategy)
import System.Taffybar.Widget.Util
  ( WindowIconWidget (..),
    computeIconStripLayout,
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
  ( getWindowIconFromClasses,
    getWindowIconFromDesktopEntryByClasses,
    pixBufFromColor,
  )

data WorkspacesConfig = WorkspacesConfig
  { workspaceStateSource :: TaffyIO (TChan WorkspaceSnapshot, MV.MVar WorkspaceSnapshot),
    widgetGap :: Int,
    iconSize :: Maybe Int32,
    maxIcons :: Maybe Int,
    minIcons :: Int,
    getWindowIconPixbuf :: Int32 -> WindowInfo -> TaffyIO (Maybe Gdk.Pixbuf),
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
    entryContents :: Gtk.Widget,
    entryIconsBox :: Gtk.Box,
    entryLabel :: Gtk.Label,
    entryWorkspaceRef :: IORef WorkspaceInfo,
    entryLastWorkspace :: WorkspaceInfo,
    entryIcons :: [WindowIconWidget WindowInfo]
  }

data WorkspaceCache = WorkspaceCache
  { cacheEntries :: M.Map WorkspaceIdentity WorkspaceEntry,
    cacheOrder :: [WorkspaceIdentity],
    cacheLastWorkspaces :: [WorkspaceInfo]
  }

defaultWorkspacesConfig :: WorkspacesConfig
defaultWorkspacesConfig =
  WorkspacesConfig
    { workspaceStateSource = getHyprlandWorkspaceStateChanAndVar,
      widgetGap = 0,
      iconSize = Just 16,
      maxIcons = Nothing,
      minIcons = 0,
      getWindowIconPixbuf = getWindowIconPixbufByClassHints,
      labelSetter = return . T.unpack . workspaceName . workspaceIdentity,
      showWorkspaceFn = \ws -> workspaceState ws /= WorkspaceEmpty && not (workspaceIsSpecial ws),
      iconSort = pure . sortWindowsByPosition,
      urgentWorkspaceState = False,
      onWorkspaceClick = defaultOnWorkspaceClick,
      onWindowClick = defaultOnWindowClick
    }

defaultEWMHWorkspacesConfig :: WorkspacesConfig
defaultEWMHWorkspacesConfig =
  defaultWorkspacesConfig
    { workspaceStateSource = getEWMHWorkspaceStateChanAndVar,
      onWorkspaceClick = defaultOnWorkspaceClickEWMH
    }

instance Default WorkspacesConfig where
  def = defaultWorkspacesConfig

sortWindowsByPosition :: [WindowInfo] -> [WindowInfo]
sortWindowsByPosition =
  sortOn $ \w ->
    ( windowMinimized w,
      fromMaybe (999999999, 999999999) (windowPosition w)
    )

getWindowIconPixbufByClassHints :: Int32 -> WindowInfo -> TaffyIO (Maybe Gdk.Pixbuf)
getWindowIconPixbufByClassHints size winInfo = tryHints classHints
  where
    classHints = map T.unpack (windowClassHints winInfo)
    tryHints [] = return Nothing
    tryHints (klass : rest) = do
      fromDesktopEntry <- getWindowIconFromDesktopEntryByClasses size klass
      case fromDesktopEntry of
        Just _ -> return fromDesktopEntry
        Nothing -> do
          fromClass <- liftIO $ getWindowIconFromClasses size klass
          case fromClass of
            Just _ -> return fromClass
            Nothing -> tryHints rest

defaultOnWorkspaceClick :: WorkspaceInfo -> TaffyIO ()
defaultOnWorkspaceClick wsInfo = do
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
  cacheVar <- liftIO $ MV.newMVar (WorkspaceCache M.empty [] [])
  (chan, snapshotVar) <- workspaceStateSource cfg
  initialSnapshot <- liftIO $ MV.readMVar snapshotVar
  renderWorkspaces cfg cont cacheVar initialSnapshot
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
      runReaderT (updateCache cfg cont snapshot oldCache) ctx

updateCache ::
  WorkspacesConfig ->
  Gtk.Box ->
  WorkspaceSnapshot ->
  WorkspaceCache ->
  TaffyIO WorkspaceCache
updateCache cfg cont snapshot oldCache = do
  let workspaces = snapshotWorkspaces snapshot
  if workspaces == cacheLastWorkspaces oldCache
    then return oldCache
    else do
      let oldEntries = cacheEntries oldCache
          buildOrUpdate (cacheAcc, entriesAcc) wsInfo = do
            let wsKey = workspaceIdentity wsInfo
            entry <-
              case M.lookup wsKey oldEntries of
                Just existing
                  | entryLastWorkspace existing == wsInfo -> return existing
                  | otherwise -> updateWorkspaceEntry cfg existing wsInfo
                Nothing -> buildWorkspaceEntry cfg wsInfo
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
            cacheLastWorkspaces = workspaces
          }

buildWorkspaceEntry :: WorkspacesConfig -> WorkspaceInfo -> TaffyIO WorkspaceEntry
buildWorkspaceEntry cfg wsInfo = do
  wsRef <- liftIO $ newIORef wsInfo
  iconsBox <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0
  label <- liftIO $ Gtk.labelNew Nothing
  _ <- widgetSetClassGI label "workspace-label"
  iconsWidget <- Gtk.toWidget iconsBox
  labelWidget <- Gtk.toWidget label
  contents <- buildWorkspaceIconLabelOverlay iconsWidget labelWidget
  button <- liftIO Gtk.eventBoxNew
  _ <- widgetSetClassGI button "workspace-button"
  liftIO $ Gtk.eventBoxSetVisibleWindow button False
  liftIO $ Gtk.containerAdd button contents
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
  updateWorkspaceEntry
    cfg
    WorkspaceEntry
      { entryWrapper = wrapper,
        entryButton = button,
        entryContents = contents,
        entryIconsBox = iconsBox,
        entryLabel = label,
        entryWorkspaceRef = wsRef,
        entryLastWorkspace = wsInfo,
        entryIcons = []
      }
    wsInfo

updateWorkspaceEntry ::
  WorkspacesConfig ->
  WorkspaceEntry ->
  WorkspaceInfo ->
  TaffyIO WorkspaceEntry
updateWorkspaceEntry cfg entry wsInfo = do
  let oldWs = entryLastWorkspace entry
  liftIO $ writeIORef (entryWorkspaceRef entry) wsInfo
  labelText <- labelSetter cfg wsInfo
  let wsState = toCSSState cfg wsInfo
  liftIO $ Gtk.labelSetMarkup (entryLabel entry) (T.pack labelText)
  liftIO $ setWorkspaceWidgetStatusClass wsState (entryContents entry)
  liftIO $ setWorkspaceWidgetStatusClass wsState (entryLabel entry)
  icons <-
    if workspaceWindows oldWs /= workspaceWindows wsInfo
      then updateWorkspaceIcons cfg (entryWorkspaceRef entry) (entryIconsBox entry) (entryIcons entry) wsInfo
      else return (entryIcons entry)
  return entry {entryLastWorkspace = wsInfo, entryIcons = icons}

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
