{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Workspaces.EWMH
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
module System.Taffybar.Widget.Workspaces.EWMH
  ( module System.Taffybar.Widget.Workspaces.EWMH,
    module System.Taffybar.Widget.Workspaces.Shared,
    module System.Taffybar.Widget.Workspaces.Config,
  )
where

import Control.Arrow ((&&&))
import Control.Concurrent
import qualified Control.Concurrent.MVar as MV
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.RateLimit
import Data.Default (Default (..))
import qualified Data.Foldable as F
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import Data.Int
import Data.List (elemIndex, intersect, sortBy, (\\))
import qualified Data.Map as M
import Data.Maybe
import qualified Data.MultiMap as MM
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Units
import Data.Tuple.Select
import Data.Tuple.Sequence
import qualified GI.Gdk.Enums as Gdk
import qualified GI.Gdk.Structs.EventScroll as Gdk
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import System.Log.Logger
import System.Taffybar.Context
import System.Taffybar.Information.EWMHDesktopInfo
import System.Taffybar.Information.SafeX11
import System.Taffybar.Information.X11DesktopInfo
import System.Taffybar.Util
import System.Taffybar.Widget.Generic.ScalingImage (getScalingImageStrategy)
import System.Taffybar.Widget.Util
import System.Taffybar.Widget.Workspaces.Config
  ( WorkspaceWidgetCommonConfig (WorkspaceWidgetCommonConfig),
  )
import qualified System.Taffybar.Widget.Workspaces.Config as WorkspaceWidgetConfig
import System.Taffybar.Widget.Workspaces.Shared
  ( WorkspaceState (..),
    buildWorkspaceIconLabelOverlay,
    cssWorkspaceStates,
    getCSSClass,
    mkWorkspaceIconWidget,
    setWorkspaceWidgetStatusClass,
  )
import System.Taffybar.WindowIcon
import Text.Printf

data WindowData = WindowData
  { windowId :: X11Window,
    windowTitle :: String,
    windowClass :: String,
    windowUrgent :: Bool,
    windowActive :: Bool,
    windowMinimized :: Bool
  }
  deriving (Show, Eq)

data WidgetUpdate = WorkspaceUpdate Workspace | IconUpdate [X11Window]

data Workspace = Workspace
  { workspaceIdx :: WorkspaceId,
    workspaceName :: String,
    workspaceState :: WorkspaceState,
    windows :: [WindowData]
  }
  deriving (Show, Eq)

data WorkspacesContext = WorkspacesContext
  { controllersVar :: MV.MVar (M.Map WorkspaceId WWC),
    workspacesVar :: MV.MVar (M.Map WorkspaceId Workspace),
    workspacesWidget :: Gtk.Box,
    ewmhConfig :: WorkspacesConfig,
    taffyContext :: Context
  }

type WorkspacesIO a = ReaderT WorkspacesContext IO a

liftContext :: TaffyIO a -> WorkspacesIO a
liftContext action = asks taffyContext >>= lift . runReaderT action

liftX11Def :: a -> X11Property a -> WorkspacesIO a
liftX11Def dflt prop = liftContext $ runX11Def dflt prop

class WorkspaceWidgetController wc where
  getWidget :: wc -> WorkspacesIO Gtk.Widget
  updateWidget :: wc -> WidgetUpdate -> WorkspacesIO wc
  updateWidgetX11 :: wc -> WidgetUpdate -> WorkspacesIO wc
  updateWidgetX11 cont _ = return cont

data WWC = forall a. (WorkspaceWidgetController a) => WWC a

instance WorkspaceWidgetController WWC where
  getWidget (WWC wc) = getWidget wc
  updateWidget (WWC wc) update = WWC <$> updateWidget wc update
  updateWidgetX11 (WWC wc) update = WWC <$> updateWidgetX11 wc update

type ControllerConstructor = Workspace -> WorkspacesIO WWC

type ParentControllerConstructor =
  ControllerConstructor -> ControllerConstructor

type WindowIconPixbufGetter =
  Int32 -> WindowData -> TaffyIO (Maybe Gdk.Pixbuf)

data WorkspacesConfig
  = WorkspacesConfig
  { workspacesConfig ::
      WorkspaceWidgetCommonConfig
        (ReaderT WorkspacesContext IO)
        Workspace
        WindowData
        WWC,
    borderWidth :: Int,
    updateEvents :: [String],
    updateRateLimitMicroseconds :: Integer
  }

workspacesCommonConfig ::
  WorkspacesConfig ->
  WorkspaceWidgetCommonConfig (ReaderT WorkspacesContext IO) Workspace WindowData WWC
workspacesCommonConfig = workspacesConfig

applyCommonWorkspacesConfig ::
  WorkspaceWidgetCommonConfig (ReaderT WorkspacesContext IO) Workspace WindowData WWC ->
  WorkspacesConfig ->
  WorkspacesConfig
applyCommonWorkspacesConfig common cfg =
  cfg {workspacesConfig = common}

defaultWorkspacesConfig :: WorkspacesConfig
defaultWorkspacesConfig =
  WorkspacesConfig
    { workspacesConfig =
        WorkspaceWidgetCommonConfig
          { WorkspaceWidgetConfig.widgetBuilder =
              buildButtonController defaultBuildContentsController,
            WorkspaceWidgetConfig.widgetGap = 0,
            WorkspaceWidgetConfig.maxIcons = Nothing,
            WorkspaceWidgetConfig.minIcons = 0,
            WorkspaceWidgetConfig.getWindowIconPixbuf = defaultGetWindowIconPixbuf,
            WorkspaceWidgetConfig.labelSetter = return . workspaceName,
            WorkspaceWidgetConfig.showWorkspaceFn = const True,
            WorkspaceWidgetConfig.iconSort = sortWindowsByPosition,
            WorkspaceWidgetConfig.urgentWorkspaceState = False
          },
      borderWidth = 2,
      updateEvents = allEWMHProperties \\ [ewmhWMIcon],
      updateRateLimitMicroseconds = 100000
    }

instance Default WorkspacesConfig where
  def = defaultWorkspacesConfig

hideEmpty :: Workspace -> Bool
hideEmpty Workspace {workspaceState = Empty} = False
hideEmpty _ = True

wLog :: (MonadIO m) => Priority -> String -> m ()
wLog l s = liftIO $ logM "System.Taffybar.Widget.Workspaces" l s

updateVar :: MV.MVar a -> (a -> WorkspacesIO a) -> WorkspacesIO a
updateVar var modify = do
  ctx <- ask
  lift $ MV.modifyMVar var $ fmap (\a -> (a, a)) . flip runReaderT ctx . modify

updateWorkspacesVar :: WorkspacesIO (M.Map WorkspaceId Workspace)
updateWorkspacesVar = do
  workspacesRef <- asks workspacesVar
  updateVar workspacesRef buildWorkspaceData

getWorkspaceToWindows ::
  [X11Window] -> X11Property (MM.MultiMap WorkspaceId X11Window)
getWorkspaceToWindows =
  foldM
    ( \theMap window ->
        MM.insert <$> getWorkspace window <*> pure window <*> pure theMap
    )
    MM.empty

getWindowData ::
  Maybe X11Window ->
  [X11Window] ->
  X11Window ->
  X11Property WindowData
getWindowData activeWindow urgentWindows window = do
  wTitle <- getWindowTitle window
  wClass <- getWindowClass window
  wMinimized <- getWindowMinimized window
  return
    WindowData
      { windowId = window,
        windowTitle = wTitle,
        windowClass = wClass,
        windowUrgent = window `elem` urgentWindows,
        windowActive = Just window == activeWindow,
        windowMinimized = wMinimized
      }

buildWorkspaceData ::
  M.Map WorkspaceId Workspace ->
  WorkspacesIO (M.Map WorkspaceId Workspace)
buildWorkspaceData _ =
  ask >>= \context -> liftX11Def M.empty $ do
    names <- getWorkspaceNames
    wins <- getWindows
    workspaceToWindows <- getWorkspaceToWindows wins
    urgentWindows <- filterM isWindowUrgent wins
    activeWindow <- getActiveWindow
    active : visible <- getVisibleWorkspaces
    let common = workspacesCommonConfig (ewmhConfig context)
        getWorkspaceState idx ws
          | idx == active = Active
          | idx `elem` visible = Visible
          | WorkspaceWidgetConfig.urgentWorkspaceState common
              && not (null (ws `intersect` urgentWindows)) =
              Urgent
          | null ws = Empty
          | otherwise = Hidden
    foldM
      ( \theMap (idx, name) -> do
          let ws = MM.lookup idx workspaceToWindows
          windowInfos <- mapM (getWindowData activeWindow urgentWindows) ws
          return $
            M.insert
              idx
              Workspace
                { workspaceIdx = idx,
                  workspaceName = name,
                  workspaceState = getWorkspaceState idx ws,
                  windows = windowInfos
                }
              theMap
      )
      M.empty
      names

addWidgetsToTopLevel :: WorkspacesIO ()
addWidgetsToTopLevel = do
  WorkspacesContext
    { controllersVar = controllersRef,
      workspacesWidget = cont
    } <-
    ask
  controllersMap <- lift $ MV.readMVar controllersRef
  -- Elems returns elements in ascending order of their keys so this will always
  -- add the widgets in the correct order
  mapM_ addWidget $ M.elems controllersMap
  lift $ Gtk.widgetShowAll cont

addWidget :: WWC -> WorkspacesIO ()
addWidget controller = do
  cont <- asks workspacesWidget
  workspaceWidget <- getWidget controller
  lift $ do
    -- XXX: This hbox exists to (hopefully) prevent the issue where workspace
    -- widgets appear out of order, in the switcher, by acting as an empty
    -- place holder when the actual widget is hidden.
    hbox <- Gtk.boxNew Gtk.OrientationHorizontal 0
    void $
      Gtk.widgetGetParent workspaceWidget
        >>= traverse (unsafeCastTo Gtk.Box)
        >>= traverse (`Gtk.containerRemove` workspaceWidget)
    Gtk.containerAdd hbox workspaceWidget
    Gtk.containerAdd cont hbox

workspacesNew :: WorkspacesConfig -> TaffyIO Gtk.Widget
workspacesNew cfg =
  ask >>= \tContext -> lift $ do
    let common = workspacesCommonConfig cfg
    cont <- Gtk.boxNew Gtk.OrientationHorizontal $ fromIntegral (WorkspaceWidgetConfig.widgetGap common)
    controllersRef <- MV.newMVar M.empty
    workspacesRef <- MV.newMVar M.empty
    let context =
          WorkspacesContext
            { controllersVar = controllersRef,
              workspacesVar = workspacesRef,
              workspacesWidget = cont,
              ewmhConfig = cfg,
              taffyContext = tContext
            }
    -- This will actually create all the widgets
    runReaderT updateAllWorkspaceWidgets context
    updateHandler <- onWorkspaceUpdate context
    iconHandler <- onIconsChanged context
    let doUpdate = lift . updateHandler
        handleConfigureEvents e@(ConfigureEvent {}) = doUpdate e
        handleConfigureEvents _ = return ()
    (workspaceSubscription, iconSubscription, geometrySubscription) <-
      flip runReaderT tContext $
        sequenceT
          ( subscribeToPropertyEvents (updateEvents cfg) doUpdate,
            subscribeToPropertyEvents [ewmhWMIcon] (lift . onIconChanged iconHandler),
            subscribeToAll handleConfigureEvents
          )
    let doUnsubscribe =
          flip runReaderT tContext $
            mapM_
              unsubscribe
              [ iconSubscription,
                workspaceSubscription,
                geometrySubscription
              ]
    _ <- Gtk.onWidgetUnrealize cont doUnsubscribe
    _ <- widgetSetClassGI cont "workspaces"
    Gtk.toWidget cont

updateAllWorkspaceWidgets :: WorkspacesIO ()
updateAllWorkspaceWidgets = do
  wLog DEBUG "Updating workspace widgets"

  workspacesMap <- updateWorkspacesVar
  wLog DEBUG $ printf "Workspaces: %s" $ show workspacesMap

  wLog DEBUG "Adding and removing widgets"
  updateWorkspaceControllers

  let updateController' idx controller =
        maybe
          (return controller)
          (updateWidget controller . WorkspaceUpdate)
          $ M.lookup idx workspacesMap
      logUpdateController i =
        wLog DEBUG $ printf "Updating %s workspace widget" $ show i
      updateController i cont =
        logUpdateController i
          >> updateController' i cont

  wLog DEBUG "Done updating individual widget"

  doWidgetUpdate updateController

  wLog DEBUG "Showing and hiding controllers"
  setControllerWidgetVisibility

setControllerWidgetVisibility :: WorkspacesIO ()
setControllerWidgetVisibility = do
  ctx@WorkspacesContext
    { workspacesVar = workspacesRef,
      controllersVar = controllersRef,
      ewmhConfig = cfg
    } <-
    ask
  let common = workspacesCommonConfig cfg
  lift $ do
    workspacesMap <- MV.readMVar workspacesRef
    controllersMap <- MV.readMVar controllersRef
    forM_ (M.elems workspacesMap) $ \ws ->
      let action =
            if WorkspaceWidgetConfig.showWorkspaceFn common ws
              then Gtk.widgetShow
              else Gtk.widgetHide
       in traverse
            (flip runReaderT ctx . getWidget)
            (M.lookup (workspaceIdx ws) controllersMap)
            >>= maybe (return ()) action

doWidgetUpdate :: (WorkspaceId -> WWC -> WorkspacesIO WWC) -> WorkspacesIO ()
doWidgetUpdate updateController = do
  c@WorkspacesContext {controllersVar = controllersRef} <- ask
  lift $ MV.modifyMVar_ controllersRef $ \controllers -> do
    wLog DEBUG "Updating controllers ref"
    controllersList <-
      mapM
        ( \(idx, controller) -> do
            newController <- runReaderT (updateController idx controller) c
            return (idx, newController)
        )
        $ M.toList controllers
    return $ M.fromList controllersList

updateWorkspaceControllers :: WorkspacesIO ()
updateWorkspaceControllers = do
  WorkspacesContext
    { controllersVar = controllersRef,
      workspacesVar = workspacesRef,
      workspacesWidget = cont,
      ewmhConfig = cfg
    } <-
    ask
  workspacesMap <- lift $ MV.readMVar workspacesRef
  controllersMap <- lift $ MV.readMVar controllersRef

  let newWorkspacesSet = M.keysSet workspacesMap
      existingWorkspacesSet = M.keysSet controllersMap
      common = workspacesCommonConfig cfg

  when (existingWorkspacesSet /= newWorkspacesSet) $ do
    let addWorkspaces = Set.difference newWorkspacesSet existingWorkspacesSet
        removeWorkspaces = Set.difference existingWorkspacesSet newWorkspacesSet
        builder = WorkspaceWidgetConfig.widgetBuilder common

    _ <- updateVar controllersRef $ \controllers -> do
      let oldRemoved = F.foldl' (flip M.delete) controllers removeWorkspaces
          buildController idx = builder <$> M.lookup idx workspacesMap
          buildAndAddController theMap idx =
            maybe
              (return theMap)
              (>>= return . flip (M.insert idx) theMap)
              (buildController idx)
      foldM buildAndAddController oldRemoved $ Set.toList addWorkspaces
    -- Clear the container and repopulate it
    lift $ Gtk.containerForeach cont (Gtk.containerRemove cont)
    addWidgetsToTopLevel

rateLimitFn ::
  forall req resp.
  WorkspacesContext ->
  (req -> IO resp) ->
  ResultsCombiner req resp ->
  IO (req -> IO resp)
rateLimitFn context =
  let limit = (updateRateLimitMicroseconds $ ewmhConfig context)
      rate = fromMicroseconds limit :: Microsecond
   in generateRateLimitedFunction $ PerInvocation rate

onWorkspaceUpdate :: WorkspacesContext -> IO (Event -> IO ())
onWorkspaceUpdate context = do
  rateLimited <- rateLimitFn context doUpdate combineRequests
  let withLog event = do
        case event of
          PropertyEvent _ _ _ _ _ atom _ _ ->
            wLog DEBUG $ printf "Event %s" $ show atom
          _anythingElse -> return ()
        void $ forkIO $ rateLimited event
  return withLog
  where
    combineRequests _ b = Just (b, const ((), ()))
    doUpdate _ = postGUIASync $ runReaderT updateAllWorkspaceWidgets context

onIconChanged :: (Set.Set X11Window -> IO ()) -> Event -> IO ()
onIconChanged handler event =
  case event of
    PropertyEvent {ev_window = wid} -> do
      wLog DEBUG $ printf "Icon changed event %s" $ show wid
      handler $ Set.singleton wid
    _ -> return ()

onIconsChanged :: WorkspacesContext -> IO (Set.Set X11Window -> IO ())
onIconsChanged context = rateLimitFn context onIconsChanged' combineRequests
  where
    combineRequests windows1 windows2 =
      Just (Set.union windows1 windows2, const ((), ()))
    onIconsChanged' wids = do
      wLog DEBUG $ printf "Icon update execute %s" $ show wids
      postGUIASync $
        flip runReaderT context $
          doWidgetUpdate
            ( \idx c ->
                wLog DEBUG (printf "Updating %s icons." $ show idx)
                  >> updateWidget c (IconUpdate $ Set.toList wids)
            )

initializeWWC ::
  (WorkspaceWidgetController a) => a -> Workspace -> ReaderT WorkspacesContext IO WWC
initializeWWC controller ws =
  WWC <$> updateWidget controller (WorkspaceUpdate ws)

-- | A WrappingController can be used to wrap some child widget with another
-- abitrary widget.
data WrappingController = WrappingController
  { wrappedWidget :: Gtk.Widget,
    wrappedController :: WWC
  }

instance WorkspaceWidgetController WrappingController where
  getWidget = lift . Gtk.toWidget . wrappedWidget
  updateWidget wc update = do
    updated <- updateWidget (wrappedController wc) update
    return wc {wrappedController = updated}

data WorkspaceContentsController = WorkspaceContentsController
  { containerWidget :: Gtk.Widget,
    contentsControllers :: [WWC]
  }

buildContentsController :: [ControllerConstructor] -> ControllerConstructor
buildContentsController constructors ws = do
  controllers <- mapM ($ ws) constructors
  ctx <- ask
  tempController <- lift $ do
    cons <- Gtk.boxNew Gtk.OrientationHorizontal 0
    mapM_ (flip runReaderT ctx . getWidget >=> Gtk.containerAdd cons) controllers
    outerBox <- Gtk.toWidget cons >>= buildPadBox
    _ <- widgetSetClassGI cons "contents"
    widget <- Gtk.toWidget outerBox
    return
      WorkspaceContentsController
        { containerWidget = widget,
          contentsControllers = controllers
        }
  initializeWWC tempController ws

defaultBuildContentsController :: ControllerConstructor
defaultBuildContentsController =
  buildContentsController [buildLabelController, buildIconController]

bottomLeftAlignedBoxWrapper :: T.Text -> ControllerConstructor -> ControllerConstructor
bottomLeftAlignedBoxWrapper boxClass constructor ws = do
  controller <- constructor ws
  widget <- getWidget controller
  wrapped <- lift $ buildBottomLeftAlignedBox boxClass widget
  let wrappingController =
        WrappingController
          { wrappedWidget = wrapped,
            wrappedController = controller
          }
  initializeWWC wrappingController ws

buildLabelOverlayController :: ControllerConstructor
buildLabelOverlayController ws = do
  iconController <- buildIconController ws
  labelController <- buildLabelController ws
  ctx <- ask
  tempController <- lift $ do
    iconWidget <- runReaderT (getWidget iconController) ctx
    labelWidget <- runReaderT (getWidget labelController) ctx
    widget <- buildWorkspaceIconLabelOverlay iconWidget labelWidget
    return
      WorkspaceContentsController
        { containerWidget = widget,
          contentsControllers = [iconController, labelController]
        }
  initializeWWC tempController ws

buildOverlayContentsController ::
  [ControllerConstructor] -> [ControllerConstructor] -> ControllerConstructor
buildOverlayContentsController mainConstructors overlayConstructors ws = do
  controllers <- mapM ($ ws) mainConstructors
  overlayControllers <- mapM ($ ws) overlayConstructors
  ctx <- ask
  tempController <- lift $ do
    mainContents <- Gtk.boxNew Gtk.OrientationHorizontal 0
    mapM_
      (flip runReaderT ctx . getWidget >=> Gtk.containerAdd mainContents)
      controllers
    outerBox <- Gtk.toWidget mainContents >>= buildPadBox
    _ <- widgetSetClassGI mainContents "contents"
    overlayWidgets <- mapM (flip runReaderT ctx . getWidget) overlayControllers
    widget <- buildOverlayWithPassThrough outerBox overlayWidgets
    return
      WorkspaceContentsController
        { containerWidget = widget,
          contentsControllers = controllers ++ overlayControllers
        }
  initializeWWC tempController ws

instance WorkspaceWidgetController WorkspaceContentsController where
  getWidget = return . containerWidget
  updateWidget cc update = do
    WorkspacesContext {} <- ask
    case update of
      WorkspaceUpdate newWorkspace ->
        lift $ setWorkspaceWidgetStatusClass (workspaceState newWorkspace) $ containerWidget cc
      _ -> return ()
    newControllers <- mapM (`updateWidget` update) $ contentsControllers cc
    return cc {contentsControllers = newControllers}
  updateWidgetX11 cc update = do
    newControllers <- mapM (`updateWidgetX11` update) $ contentsControllers cc
    return cc {contentsControllers = newControllers}

newtype LabelController = LabelController {label :: Gtk.Label}

buildLabelController :: ControllerConstructor
buildLabelController ws = do
  tempController <- lift $ do
    lbl <- Gtk.labelNew Nothing
    _ <- widgetSetClassGI lbl "workspace-label"
    return LabelController {label = lbl}
  initializeWWC tempController ws

instance WorkspaceWidgetController LabelController where
  getWidget = lift . Gtk.toWidget . label
  updateWidget lc (WorkspaceUpdate newWorkspace) = do
    WorkspacesContext {ewmhConfig = cfg} <- ask
    let common = workspacesCommonConfig cfg
    labelText <- WorkspaceWidgetConfig.labelSetter common newWorkspace
    lift $ do
      Gtk.labelSetMarkup (label lc) $ T.pack labelText
      setWorkspaceWidgetStatusClass (workspaceState newWorkspace) $ label lc
    return lc
  updateWidget lc _ = return lc

type IconWidget = WindowIconWidget WindowData

buildIconWidget :: Bool -> Workspace -> WorkspacesIO IconWidget
buildIconWidget transparentOnNone ws = do
  ctx <- ask
  let tContext = taffyContext ctx
      cfg = ewmhConfig ctx
      common = workspacesCommonConfig cfg
      getPB size windowData =
        runReaderT (WorkspaceWidgetConfig.getWindowIconPixbuf common size windowData) tContext
  strategy <- liftContext getScalingImageStrategy
  lift $ do
    iconWidget <-
      mkWorkspaceIconWidget
        strategy
        Nothing
        transparentOnNone
        getPB
        (`pixBufFromColor` 0)
    _ <-
      Gtk.onWidgetButtonPressEvent (iconContainer iconWidget) $
        const $
          liftIO $ do
            info <- MV.readMVar (iconWindow iconWidget)
            case info of
              Just updatedInfo ->
                flip runReaderT ctx $
                  liftX11Def () $
                    focusWindow $
                      windowId updatedInfo
              _ -> liftIO $ void $ switch ctx (workspaceIdx ws)
            return True
    return iconWidget

data IconController = IconController
  { iconsContainer :: Gtk.Box,
    iconImages :: [IconWidget],
    iconWorkspace :: Workspace
  }

buildIconController :: ControllerConstructor
buildIconController ws = do
  tempController <-
    lift $ do
      hbox <- Gtk.boxNew Gtk.OrientationHorizontal 0
      return
        IconController
          { iconsContainer = hbox,
            iconImages = [],
            iconWorkspace = ws
          }
  initializeWWC tempController ws

instance WorkspaceWidgetController IconController where
  getWidget = lift . Gtk.toWidget . iconsContainer
  updateWidget ic (WorkspaceUpdate newWorkspace) = do
    newImages <- updateImages ic newWorkspace
    return ic {iconImages = newImages, iconWorkspace = newWorkspace}
  updateWidget ic (IconUpdate updatedIcons) =
    updateWindowIconsById ic updatedIcons >> return ic

updateWindowIconsById ::
  IconController -> [X11Window] -> WorkspacesIO ()
updateWindowIconsById ic windowIds =
  mapM_ maybeUpdateWindowIcon $ iconImages ic
  where
    maybeUpdateWindowIcon widget =
      do
        info <- lift $ MV.readMVar $ iconWindow widget
        when (maybe False (flip elem windowIds . windowId) info) $
          updateIconWidget ic widget info

scaledWindowIconPixbufGetter :: WindowIconPixbufGetter -> WindowIconPixbufGetter
scaledWindowIconPixbufGetter = scaledPixbufGetter

constantScaleWindowIconPixbufGetter ::
  Int32 -> WindowIconPixbufGetter -> WindowIconPixbufGetter
constantScaleWindowIconPixbufGetter constantSize getter =
  const $ scaledWindowIconPixbufGetter getter constantSize

handleIconGetterException :: WindowIconPixbufGetter -> WindowIconPixbufGetter
handleIconGetterException = handlePixbufGetterException wLog

getWindowIconPixbufFromEWMH :: WindowIconPixbufGetter
getWindowIconPixbufFromEWMH = handleIconGetterException $ \size windowData ->
  runX11Def Nothing (getIconPixBufFromEWMH size $ windowId windowData)

getWindowIconPixbufFromClass :: WindowIconPixbufGetter
getWindowIconPixbufFromClass = handleIconGetterException $ \size windowData ->
  lift $ getWindowIconFromClasses size (windowClass windowData)

getWindowIconPixbufFromDesktopEntry :: WindowIconPixbufGetter
getWindowIconPixbufFromDesktopEntry = handleIconGetterException $ \size windowData ->
  getWindowIconFromDesktopEntryByClasses size (windowClass windowData)

getWindowIconPixbufFromChrome :: WindowIconPixbufGetter
getWindowIconPixbufFromChrome _ windowData =
  getPixBufFromChromeData $ windowId windowData

defaultGetWindowIconPixbuf :: WindowIconPixbufGetter
defaultGetWindowIconPixbuf =
  scaledWindowIconPixbufGetter unscaledDefaultGetWindowIconPixbuf

unscaledDefaultGetWindowIconPixbuf :: WindowIconPixbufGetter
unscaledDefaultGetWindowIconPixbuf =
  getWindowIconPixbufFromDesktopEntry
    <|||> getWindowIconPixbufFromClass
    <|||> getWindowIconPixbufFromEWMH

addCustomIconsToDefaultWithFallbackByPath ::
  (WindowData -> Maybe FilePath) ->
  FilePath ->
  WindowIconPixbufGetter
addCustomIconsToDefaultWithFallbackByPath getCustomIconPath fallbackPath =
  addCustomIconsAndFallback
    getCustomIconPath
    (const $ lift $ getPixbufFromFilePath fallbackPath)
    unscaledDefaultGetWindowIconPixbuf

addCustomIconsAndFallback ::
  (WindowData -> Maybe FilePath) ->
  (Int32 -> TaffyIO (Maybe Gdk.Pixbuf)) ->
  WindowIconPixbufGetter ->
  WindowIconPixbufGetter
addCustomIconsAndFallback getCustomIconPath fallback defaultGetter =
  scaledWindowIconPixbufGetter $
    getCustomIcon <|||> defaultGetter <|||> (\s _ -> fallback s)
  where
    getCustomIcon :: Int32 -> WindowData -> TaffyIO (Maybe Gdk.Pixbuf)
    getCustomIcon _ wdata =
      lift $
        maybe (return Nothing) getPixbufFromFilePath $
          getCustomIconPath wdata

-- | Sort windows by top-left corner position.
sortWindowsByPosition :: [WindowData] -> WorkspacesIO [WindowData]
sortWindowsByPosition wins = do
  let getGeometryWorkspaces w = getDisplay >>= liftIO . (`safeGetGeometry` w)
      getGeometries =
        mapM
          ( forkM
              return
              (((sel2 &&& sel3) <$>) . getGeometryWorkspaces)
              . windowId
          )
          wins
  windowGeometries <- liftX11Def [] getGeometries
  let getLeftPos wd =
        fromMaybe (999999999, 99999999) $ lookup (windowId wd) windowGeometries
      compareWindowData a b =
        compare
          (windowMinimized a, getLeftPos a)
          (windowMinimized b, getLeftPos b)
  return $ sortBy compareWindowData wins

-- | Sort windows in reverse _NET_CLIENT_LIST_STACKING order.
-- Starting in xmonad-contrib 0.17.0, this is effectively focus history, active first.
-- Previous versions erroneously stored focus-sort-order in _NET_CLIENT_LIST.
sortWindowsByStackIndex :: [WindowData] -> WorkspacesIO [WindowData]
sortWindowsByStackIndex wins = do
  stackingWindows <- liftX11Def [] getWindowsStacking
  let getStackIdx wd = fromMaybe (-1) $ elemIndex (windowId wd) stackingWindows
      compareWindowData a b = compare (getStackIdx b) (getStackIdx a)
  return $ sortBy compareWindowData wins

updateImages :: IconController -> Workspace -> WorkspacesIO [IconWidget]
updateImages ic ws = do
  WorkspacesContext {ewmhConfig = cfg} <- ask
  let common = workspacesCommonConfig cfg
  sortedWindows <- WorkspaceWidgetConfig.iconSort common $ windows ws
  wLog DEBUG $ printf "Updating images for %s" (show ws)
  let (effectiveMinIcons, _targetLen, paddedWindows) =
        computeIconStripLayout
          (WorkspaceWidgetConfig.minIcons common)
          (WorkspaceWidgetConfig.maxIcons common)
          sortedWindows
      buildOne i = buildIconWidget (i < effectiveMinIcons) ws
      updateOne = updateIconWidget ic
  syncWidgetPool (iconsContainer ic) (iconImages ic) paddedWindows buildOne iconContainer updateOne

getWindowStatusString :: WindowData -> T.Text
getWindowStatusString windowData =
  windowStatusClassFromFlags
    (windowMinimized windowData)
    (windowActive windowData)
    (windowUrgent windowData)

updateIconWidget ::
  IconController ->
  IconWidget ->
  Maybe WindowData ->
  WorkspacesIO ()
updateIconWidget _ iconWidget windowData =
  updateWindowIconWidgetState
    iconWidget
    windowData
    (T.pack . windowTitle)
    getWindowStatusString

data WorkspaceButtonController = WorkspaceButtonController
  { button :: Gtk.EventBox,
    buttonWorkspace :: Workspace,
    contentsController :: WWC
  }

buildButtonController :: ParentControllerConstructor
buildButtonController contentsBuilder workspace = do
  cc <- contentsBuilder workspace
  workspacesRef <- asks workspacesVar
  ctx <- ask
  widget <- getWidget cc
  lift $ do
    ebox <- Gtk.eventBoxNew
    Gtk.containerAdd ebox widget
    Gtk.eventBoxSetVisibleWindow ebox False
    _ <-
      Gtk.onWidgetScrollEvent ebox $ \scrollEvent -> do
        dir <- Gdk.getEventScrollDirection scrollEvent
        workspaces <- liftIO $ MV.readMVar workspacesRef
        let switchOne a =
              liftIO $
                flip runReaderT ctx $
                  liftX11Def
                    ()
                    (switchOneWorkspace a (length (M.toList workspaces) - 1))
                    >> return True
        case dir of
          Gdk.ScrollDirectionUp -> switchOne True
          Gdk.ScrollDirectionLeft -> switchOne True
          Gdk.ScrollDirectionDown -> switchOne False
          Gdk.ScrollDirectionRight -> switchOne False
          _ -> return False
    _ <- Gtk.onWidgetButtonPressEvent ebox $ const $ switch ctx $ workspaceIdx workspace
    return $
      WWC
        WorkspaceButtonController
          { button = ebox,
            buttonWorkspace = workspace,
            contentsController = cc
          }

switch :: (MonadIO m) => WorkspacesContext -> WorkspaceId -> m Bool
switch ctx idx = do
  liftIO $ flip runReaderT ctx $ liftX11Def () $ switchToWorkspace idx
  return True

instance WorkspaceWidgetController WorkspaceButtonController where
  getWidget wbc = lift $ Gtk.toWidget $ button wbc
  updateWidget wbc update = do
    newContents <- updateWidget (contentsController wbc) update
    return wbc {contentsController = newContents}
