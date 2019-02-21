{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, RankNTypes, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.Workspaces
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------

module System.Taffybar.Widget.Workspaces where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Concurrent
import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.RateLimit
import qualified Data.Foldable as F
import           Data.Int
import           Data.List (intersect, sortBy)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.MultiMap as MM
import           Data.Ord
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Time.Units
import           Data.Tuple.Select
import           Data.Tuple.Sequence
import qualified GI.Gdk.Enums as Gdk
import qualified GI.Gdk.Structs.EventScroll as Gdk
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import           Prelude
import           StatusNotifier.Tray (scalePixbufToSize)
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.Information.EWMHDesktopInfo
import           System.Taffybar.Information.SafeX11
import           System.Taffybar.Information.X11DesktopInfo
import           System.Taffybar.Util
import           System.Taffybar.Widget.Decorators
import           System.Taffybar.Widget.Generic.AutoSizeImage (autoSizeImage)
import           System.Taffybar.Widget.Util
import           System.Taffybar.WindowIcon
import           Text.Printf

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

data WindowData = WindowData
  { windowId :: X11Window
  , windowTitle :: String
  , windowClass :: String
  , windowUrgent :: Bool
  , windowActive :: Bool
  , windowMinimized :: Bool
  } deriving (Show, Eq)

data WidgetUpdate = WorkspaceUpdate Workspace | IconUpdate [X11Window]

data Workspace = Workspace
  { workspaceIdx :: WorkspaceIdx
  , workspaceName :: String
  , workspaceState :: WorkspaceState
  , windows :: [WindowData]
  } deriving (Show, Eq)

data WorkspacesContext = WorkspacesContext
  { controllersVar :: MV.MVar (M.Map WorkspaceIdx WWC)
  , workspacesVar :: MV.MVar (M.Map WorkspaceIdx Workspace)
  , workspacesWidget :: Gtk.Box
  , workspacesConfig :: WorkspacesConfig
  , taffyContext :: Context
  }

type WorkspacesIO a = ReaderT WorkspacesContext IO a

liftContext :: TaffyIO a -> WorkspacesIO a
liftContext action = asks taffyContext >>= lift . runReaderT action

liftX11Def :: a -> X11Property a -> WorkspacesIO a
liftX11Def def prop = liftContext $ runX11Def def prop

setWorkspaceWidgetStatusClass ::
     (MonadIO m, Gtk.IsWidget a) => Workspace -> a -> m ()
setWorkspaceWidgetStatusClass workspace widget =
  updateWidgetClasses
    widget
    [getCSSClass $ workspaceState workspace]
    cssWorkspaceStates

updateWidgetClasses ::
  (Foldable t1, Foldable t, Gtk.IsWidget a, MonadIO m)
  => a
  -> t1 T.Text
  -> t T.Text
  -> m ()
updateWidgetClasses widget toAdd toRemove = do
  context <- Gtk.widgetGetStyleContext widget
  let hasClass = Gtk.styleContextHasClass context
      addIfMissing klass =
        hasClass klass >>= (`when` Gtk.styleContextAddClass context klass) . not
      removeIfPresent klass = unless (klass `elem` toAdd) $
        hasClass klass >>= (`when` Gtk.styleContextRemoveClass context klass)
  mapM_ removeIfPresent toRemove
  mapM_ addIfMissing toAdd

class WorkspaceWidgetController wc where
  getWidget :: wc -> WorkspacesIO Gtk.Widget
  updateWidget :: wc -> WidgetUpdate -> WorkspacesIO wc
  updateWidgetX11 :: wc -> WidgetUpdate -> WorkspacesIO wc
  updateWidgetX11 cont _ = return cont

data WWC = forall a. WorkspaceWidgetController a => WWC a

instance WorkspaceWidgetController WWC where
  getWidget (WWC wc) = getWidget wc
  updateWidget (WWC wc) update = WWC <$> updateWidget wc update
  updateWidgetX11 (WWC wc) update = WWC <$> updateWidgetX11 wc update

type ControllerConstructor = Workspace -> WorkspacesIO WWC
type ParentControllerConstructor =
  ControllerConstructor -> ControllerConstructor

type WindowIconPixbufGetter =
  Int32 -> WindowData -> TaffyIO (Maybe Gdk.Pixbuf)

data WorkspacesConfig =
  WorkspacesConfig
  { widgetBuilder :: ControllerConstructor
  , widgetGap :: Int
  , underlineHeight :: Int
  , underlinePadding :: Int
  , maxIcons :: Maybe Int
  , minIcons :: Int
  , getWindowIconPixbuf :: WindowIconPixbufGetter
  , labelSetter :: Workspace -> WorkspacesIO String
  , showWorkspaceFn :: Workspace -> Bool
  , borderWidth :: Int
  , updateEvents :: [String]
  , updateRateLimitMicroseconds :: Integer
  , iconSort :: [WindowData] -> WorkspacesIO [WindowData]
  , urgentWorkspaceState :: Bool
  }

defaultWorkspacesConfig :: WorkspacesConfig
defaultWorkspacesConfig =
  WorkspacesConfig
  { widgetBuilder = buildButtonController defaultBuildContentsController
  , widgetGap = 0
  , underlineHeight = 4
  , underlinePadding = 1
  , maxIcons = Nothing
  , minIcons = 0
  , getWindowIconPixbuf = defaultGetWindowIconPixbuf
  , labelSetter = return . workspaceName
  , showWorkspaceFn = const True
  , borderWidth = 2
  , iconSort = sortWindowsByPosition
  , updateEvents =
      [ "WM_HINTS"
      , "_NET_CURRENT_DESKTOP"
      , "_NET_DESKTOP_NAMES"
      , "_NET_NUMBER_OF_DESKTOPS"
      , "_NET_WM_DESKTOP"
      , "_NET_WM_STATE_HIDDEN"
      ]
  , updateRateLimitMicroseconds = 100000
  , urgentWorkspaceState = False
  }

hideEmpty :: Workspace -> Bool
hideEmpty Workspace { workspaceState = Empty } = False
hideEmpty _ = True

wLog :: MonadIO m => Priority -> String -> m ()
wLog l s = liftIO $ logM "System.Taffybar.Widget.Workspaces" l s

updateVar :: MV.MVar a -> (a -> WorkspacesIO a) -> WorkspacesIO a
updateVar var modify = do
  ctx <- ask
  lift $ MV.modifyMVar var $ fmap (\a -> (a, a)) . flip runReaderT ctx . modify

updateWorkspacesVar :: WorkspacesIO (M.Map WorkspaceIdx Workspace)
updateWorkspacesVar = do
  workspacesRef <- asks workspacesVar
  updateVar workspacesRef buildWorkspaceData

getWorkspaceToWindows :: [X11Window] -> X11Property (MM.MultiMap WorkspaceIdx X11Window)
getWorkspaceToWindows =
  foldM
    (\theMap window ->
       MM.insert <$> getWorkspace window <*> pure window <*> pure theMap)
    MM.empty

getWindowData :: [X11Window]
              -> [X11Window]
              -> X11Window
              -> X11Property WindowData
getWindowData activeWindows urgentWindows window = do
  wTitle <- getWindowTitle window
  wClass <- getWindowClass window
  wMinimized <- getWindowStateProperty window "_NET_WM_STATE_HIDDEN"
  return
    WindowData
    { windowId = window
    , windowTitle = wTitle
    , windowClass = wClass
    , windowUrgent = window `elem` urgentWindows
    , windowActive = window `elem` activeWindows
    , windowMinimized = wMinimized
    }

buildWorkspaceData :: M.Map WorkspaceIdx Workspace
                -> WorkspacesIO (M.Map WorkspaceIdx Workspace)
buildWorkspaceData _ = ask >>= \context -> liftX11Def M.empty $ do
  names <- getWorkspaceNames
  wins <- getWindows
  workspaceToWindows <- getWorkspaceToWindows wins
  urgentWindows <- filterM isWindowUrgent wins
  activeWindows <- readAsListOfWindow Nothing "_NET_ACTIVE_WINDOW"
  active:visible <- getVisibleWorkspaces
  let getWorkspaceState idx ws
        | idx == active = Active
        | idx `elem` visible = Visible
        | urgentWorkspaceState (workspacesConfig context) &&
          not (null (ws `intersect` urgentWindows)) =
          Urgent
        | null ws = Empty
        | otherwise = Hidden
  foldM
    (\theMap (idx, name) -> do
       let ws = MM.lookup idx workspaceToWindows
       windowInfos <- mapM (getWindowData activeWindows urgentWindows) ws
       return $
         M.insert
           idx
           Workspace
           { workspaceIdx = idx
           , workspaceName = name
           , workspaceState = getWorkspaceState idx ws
           , windows = windowInfos
           }
           theMap)
    M.empty
    names

addWidgetsToTopLevel :: WorkspacesIO ()
addWidgetsToTopLevel = do
  WorkspacesContext
    { controllersVar = controllersRef
    , workspacesWidget = cont
    } <- ask
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
    parent <- Gtk.widgetGetParent workspaceWidget
    if isJust parent
      then Gtk.widgetReparent workspaceWidget hbox
      else Gtk.containerAdd hbox workspaceWidget
    Gtk.containerAdd cont hbox

workspacesNew :: WorkspacesConfig -> TaffyIO Gtk.Widget
workspacesNew cfg = ask >>= \tContext -> lift $ do
  cont <- Gtk.boxNew Gtk.OrientationHorizontal $ fromIntegral (widgetGap cfg)
  controllersRef <- MV.newMVar M.empty
  workspacesRef <- MV.newMVar M.empty
  let context =
        WorkspacesContext
        { controllersVar = controllersRef
        , workspacesVar = workspacesRef
        , workspacesWidget = cont
        , workspacesConfig = cfg
        , taffyContext = tContext
        }
  -- This will actually create all the widgets
  runReaderT updateAllWorkspaceWidgets context
  updateHandler <- onWorkspaceUpdate context
  iconHandler <- onIconsChanged context
  (workspaceSubscription, iconSubscription) <-
    flip runReaderT tContext $ sequenceT
         ( subscribeToEvents (updateEvents cfg) $ lift . updateHandler
         , subscribeToEvents ["_NET_WM_ICON"] (lift . onIconChanged iconHandler)
         )
  let doUnsubscribe = flip runReaderT tContext $
        mapM_ unsubscribe [iconSubscription, workspaceSubscription]
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
        maybe (return controller)
              (updateWidget controller . WorkspaceUpdate) $
              M.lookup idx workspacesMap
      logUpdateController i =
        wLog DEBUG $ printf "Updating %s workspace widget" $ show i
      updateController i cont = logUpdateController i >>
                                updateController' i cont

  wLog DEBUG "Done updating individual widget"

  doWidgetUpdate updateController

  wLog DEBUG "Showing and hiding controllers"
  setControllerWidgetVisibility

setControllerWidgetVisibility :: WorkspacesIO ()
setControllerWidgetVisibility = do
  ctx@WorkspacesContext
    { workspacesVar = workspacesRef
    , controllersVar = controllersRef
    , workspacesConfig = cfg
    } <- ask
  lift $ do
    workspacesMap <- MV.readMVar workspacesRef
    controllersMap <- MV.readMVar controllersRef
    forM_ (M.elems workspacesMap) $ \ws ->
      let action = if showWorkspaceFn cfg ws
                   then Gtk.widgetShow
                   else Gtk.widgetHide
      in
        traverse (flip runReaderT ctx . getWidget)
                    (M.lookup (workspaceIdx ws) controllersMap) >>=
        maybe (return ()) action

doWidgetUpdate :: (WorkspaceIdx -> WWC -> WorkspacesIO WWC) -> WorkspacesIO ()
doWidgetUpdate updateController = do
  c@WorkspacesContext { controllersVar = controllersRef } <- ask
  lift $ MV.modifyMVar_ controllersRef $ \controllers -> do
    wLog DEBUG "Updating controllers ref"
    controllersList <-
      mapM
      (\(idx, controller) -> do
         newController <- runReaderT (updateController idx controller) c
         return (idx, newController)) $
      M.toList controllers
    return $ M.fromList controllersList

updateWorkspaceControllers :: WorkspacesIO ()
updateWorkspaceControllers = do
  WorkspacesContext
    { controllersVar = controllersRef
    , workspacesVar = workspacesRef
    , workspacesWidget = cont
    , workspacesConfig = cfg
    } <- ask
  workspacesMap <- lift $ MV.readMVar workspacesRef
  controllersMap <- lift $ MV.readMVar controllersRef

  let newWorkspacesSet = M.keysSet workspacesMap
      existingWorkspacesSet = M.keysSet controllersMap

  when (existingWorkspacesSet /= newWorkspacesSet) $ do
    let addWorkspaces = Set.difference newWorkspacesSet existingWorkspacesSet
        removeWorkspaces = Set.difference existingWorkspacesSet newWorkspacesSet
        builder = widgetBuilder cfg

    _ <- updateVar controllersRef $ \controllers -> do
      let oldRemoved = F.foldl (flip M.delete) controllers removeWorkspaces
          buildController idx = builder <$> M.lookup idx workspacesMap
          buildAndAddController theMap idx =
            maybe (return theMap) (>>= return . flip (M.insert idx) theMap)
                    (buildController idx)
      foldM buildAndAddController oldRemoved $ Set.toList addWorkspaces
    -- Clear the container and repopulate it
    lift $ Gtk.containerForeach cont (Gtk.containerRemove cont)
    addWidgetsToTopLevel

rateLimitFn
  :: forall req resp.
     WorkspacesContext
  -> (req -> IO resp)
  -> ResultsCombiner req resp
  -> IO (req -> IO resp)
rateLimitFn context =
  let limit = (updateRateLimitMicroseconds $ workspacesConfig context)
      rate = fromMicroseconds limit :: Microsecond in
  generateRateLimitedFunction $ PerInvocation rate

onWorkspaceUpdate :: WorkspacesContext -> IO (Event -> IO ())
onWorkspaceUpdate context = do
  rateLimited <- rateLimitFn context doUpdate combineRequests
  let withLog event = do
        case event of
          PropertyEvent _ _ _ _ _ atom _ _ ->
            wLog DEBUG $ printf "Event %s" $ show atom
          _ -> return ()
        void $ forkIO $ rateLimited event
  return withLog
  where
    combineRequests _ b = Just (b, const ((), ()))
    doUpdate _ = postGUIASync $ runReaderT updateAllWorkspaceWidgets context

onIconChanged :: (Set.Set X11Window -> IO ()) -> Event -> IO ()
onIconChanged handler event =
  case event of
    PropertyEvent { ev_window = wid } -> do
      wLog DEBUG  $ printf "Icon changed event %s" $ show wid
      handler $ Set.singleton wid
    _ -> return ()

onIconsChanged :: WorkspacesContext -> IO (Set.Set X11Window -> IO ())
onIconsChanged context = rateLimitFn context onIconsChanged' combineRequests
  where
    combineRequests windows1 windows2 =
      Just (Set.union windows1 windows2, const ((), ()))
    onIconsChanged' wids = do
      wLog DEBUG $ printf "Icon update execute %s" $ show wids
      postGUIASync $ flip runReaderT context $
        doWidgetUpdate
          (\idx c ->
             wLog DEBUG (printf "Updating %s icons." $ show idx) >>
             updateWidget c (IconUpdate $ Set.toList wids))

data WorkspaceContentsController = WorkspaceContentsController
  { containerWidget :: Gtk.Widget
  , contentsControllers :: [WWC]
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
      { containerWidget = widget
      , contentsControllers = controllers
      }
  WWC <$> updateWidget tempController (WorkspaceUpdate ws)

defaultBuildContentsController :: ControllerConstructor
defaultBuildContentsController =
  buildContentsController [buildLabelController, buildIconController]

instance WorkspaceWidgetController WorkspaceContentsController where
  getWidget = return . containerWidget
  updateWidget cc update = do
    WorkspacesContext {} <- ask
    case update of
      WorkspaceUpdate newWorkspace ->
        lift $ setWorkspaceWidgetStatusClass newWorkspace $ containerWidget cc
      _ -> return ()
    newControllers <- mapM (`updateWidget` update) $ contentsControllers cc
    return cc {contentsControllers = newControllers}
  updateWidgetX11 cc update = do
    newControllers <- mapM (`updateWidgetX11` update) $ contentsControllers cc
    return cc {contentsControllers = newControllers}

newtype LabelController = LabelController { label :: Gtk.Label }

buildLabelController :: ControllerConstructor
buildLabelController ws = do
  tempController <- lift $ do
    lbl <- Gtk.labelNew Nothing
    _ <- widgetSetClassGI lbl "workspace-label"
    return LabelController { label = lbl }
  WWC <$> updateWidget tempController (WorkspaceUpdate ws)

instance WorkspaceWidgetController LabelController where
  getWidget = lift . Gtk.toWidget . label
  updateWidget lc (WorkspaceUpdate newWorkspace) = do
    WorkspacesContext { workspacesConfig = cfg } <- ask
    labelText <- labelSetter cfg newWorkspace
    lift $ do
      Gtk.labelSetMarkup (label lc) $ T.pack labelText
      setWorkspaceWidgetStatusClass newWorkspace $ label lc
    return lc
  updateWidget lc _ = return lc

data IconWidget = IconWidget
  { iconContainer :: Gtk.EventBox
  , iconImage :: Gtk.Image
  , iconWindow :: MV.MVar (Maybe WindowData)
  , iconForceUpdate :: IO ()
  }

getPixbufForIconWidget :: Bool
                       -> MV.MVar (Maybe WindowData)
                       -> Int32
                       -> WorkspacesIO (Maybe Gdk.Pixbuf)
getPixbufForIconWidget transparentOnNone dataVar size = do
  ctx <- ask
  let tContext = taffyContext ctx
      getPBFromData = getWindowIconPixbuf $ workspacesConfig ctx
      getPB' = runMaybeT $
               MaybeT (lift $ MV.readMVar dataVar) >>= MaybeT . getPBFromData size
      getPB = if transparentOnNone
              then maybeTCombine getPB' (Just <$> pixBufFromColor size 0)
              else getPB'
  lift $ runReaderT getPB tContext

buildIconWidget :: Bool -> Workspace -> WorkspacesIO IconWidget
buildIconWidget transparentOnNone ws = do
  ctx <- ask
  lift $ do
    windowVar <- MV.newMVar Nothing
    img <- Gtk.imageNew
    refreshImage <-
      autoSizeImage img
        (flip runReaderT ctx . getPixbufForIconWidget transparentOnNone windowVar)
        Gtk.OrientationHorizontal
    ebox <- Gtk.eventBoxNew
    _ <- widgetSetClassGI img "window-icon"
    _ <- widgetSetClassGI ebox "window-icon-container"
    Gtk.containerAdd ebox img
    _ <-
      Gtk.onWidgetButtonPressEvent ebox $
      const $ liftIO $ do
        info <- MV.readMVar windowVar
        case info of
          Just updatedInfo ->
            flip runReaderT ctx $
            liftX11Def () $ focusWindow $ windowId updatedInfo
          _ -> liftIO $ void $ switch ctx (workspaceIdx ws)
        return True
    return
      IconWidget
      { iconContainer = ebox
      , iconImage = img
      , iconWindow = windowVar
      , iconForceUpdate = refreshImage
      }

data IconController = IconController
  { iconsContainer :: Gtk.Box
  , iconImages :: [IconWidget]
  , iconWorkspace :: Workspace
  }

buildIconController :: ControllerConstructor
buildIconController ws = do
  tempController <-
    lift $ do
      hbox <- Gtk.boxNew Gtk.OrientationHorizontal 0
      return
        IconController
        {iconsContainer = hbox, iconImages = [], iconWorkspace = ws}
  WWC <$> updateWidget tempController (WorkspaceUpdate ws)

instance WorkspaceWidgetController IconController where
  getWidget = lift . Gtk.toWidget . iconsContainer
  updateWidget ic (WorkspaceUpdate newWorkspace) = do
    newImages <- updateImages ic newWorkspace
    return ic { iconImages = newImages, iconWorkspace = newWorkspace }
  updateWidget ic (IconUpdate updatedIcons) =
    updateWindowIconsById ic updatedIcons >> return ic

updateWindowIconsById :: IconController
                      -> [X11Window]
                      -> WorkspacesIO ()
updateWindowIconsById ic windowIds =
  mapM_ maybeUpdateWindowIcon $ iconImages ic
  where
    maybeUpdateWindowIcon widget =
      do
        info <- lift $ MV.readMVar $ iconWindow widget
        when (maybe False (flip elem windowIds . windowId) info) $
         updateIconWidget ic widget info

scaledWindowIconPixbufGetter :: WindowIconPixbufGetter -> WindowIconPixbufGetter
scaledWindowIconPixbufGetter getter size =
  getter size >=>
  lift . traverse (scalePixbufToSize size Gtk.OrientationHorizontal)

constantScaleWindowIconPixbufGetter :: Int32
                                    -> WindowIconPixbufGetter
                                    -> WindowIconPixbufGetter
constantScaleWindowIconPixbufGetter constantSize getter =
  const $ scaledWindowIconPixbufGetter getter constantSize

getWindowIconPixbufFromEWMH :: WindowIconPixbufGetter
getWindowIconPixbufFromEWMH size windowData =
  runX11Def Nothing (getIconPixBufFromEWMH size $ windowId windowData)

getWindowIconPixbufFromClass :: WindowIconPixbufGetter
getWindowIconPixbufFromClass size windowData =
  lift $ getWindowIconFromClasses size (windowClass windowData)

getWindowIconPixbufFromDesktopEntry :: WindowIconPixbufGetter
getWindowIconPixbufFromDesktopEntry size windowData =
  getWindowIconFromDesktopEntryByClasses size (windowClass windowData)

getWindowIconPixbufFromChrome :: WindowIconPixbufGetter
getWindowIconPixbufFromChrome _ windowData =
  getPixBufFromChromeData $ windowId windowData

defaultGetWindowIconPixbuf :: WindowIconPixbufGetter
defaultGetWindowIconPixbuf =
  scaledWindowIconPixbufGetter unscaledDefaultGetWindowIconPixbuf

unscaledDefaultGetWindowIconPixbuf :: WindowIconPixbufGetter
unscaledDefaultGetWindowIconPixbuf =
  getWindowIconPixbufFromDesktopEntry <|||>
  getWindowIconPixbufFromClass <|||>
  getWindowIconPixbufFromEWMH

addCustomIconsToDefaultWithFallbackByPath
  :: (WindowData -> Maybe FilePath)
  -> FilePath
  -> WindowIconPixbufGetter
addCustomIconsToDefaultWithFallbackByPath getCustomIconPath fallbackPath =
  addCustomIconsAndFallback
    getCustomIconPath
    (const $ lift $ getPixbufFromFilePath fallbackPath)
    unscaledDefaultGetWindowIconPixbuf

addCustomIconsAndFallback
  :: (WindowData -> Maybe FilePath)
  -> (Int32 -> TaffyIO (Maybe Gdk.Pixbuf))
  -> WindowIconPixbufGetter
  -> WindowIconPixbufGetter
addCustomIconsAndFallback getCustomIconPath fallback defaultGetter =
  scaledWindowIconPixbufGetter $
  getCustomIcon <|||> defaultGetter <|||> (\s _ -> fallback s)
  where
    getCustomIcon :: Int32 -> WindowData -> TaffyIO (Maybe Gdk.Pixbuf)
    getCustomIcon _ wdata =
      lift $
      maybe (return Nothing) getPixbufFromFilePath $ getCustomIconPath wdata

sortWindowsByPosition :: [WindowData] -> WorkspacesIO [WindowData]
sortWindowsByPosition wins = do
  let getGeometryWorkspaces w = getDisplay >>= liftIO . (`safeGetGeometry` w)
      getGeometries = mapM
                      (forkM return ((((sel2 &&& sel3) <$>) .) getGeometryWorkspaces) . windowId)
                      wins
  windowGeometries <- liftX11Def [] getGeometries
  let getLeftPos wd =
        fromMaybe (999999999, 99999999) $ lookup (windowId wd) windowGeometries
      compareWindowData a b =
        compare
          (windowMinimized a, getLeftPos a)
          (windowMinimized b, getLeftPos b)
  return $ sortBy compareWindowData wins

updateImages :: IconController -> Workspace -> WorkspacesIO [IconWidget]
updateImages ic ws = do
  WorkspacesContext {workspacesConfig = cfg} <- ask
  sortedWindows <- iconSort cfg $ windows ws
  wLog DEBUG $ printf "Updating images for %s" (show ws)
  let updateIconWidget' getImageAction wdata = do
        iconWidget <- getImageAction
        _ <- updateIconWidget ic iconWidget wdata
        return iconWidget
      existingImages = map return $ iconImages ic
      buildAndAddIconWidget transparentOnNone = do
        iw <- buildIconWidget transparentOnNone ws
        lift $ Gtk.containerAdd (iconsContainer ic) $ iconContainer iw
        return iw
      infiniteImages =
        existingImages ++
        replicate (minIcons cfg - length existingImages)
                  (buildAndAddIconWidget True) ++
        repeat (buildAndAddIconWidget False)
      windowCount = length $ windows ws
      maxNeeded = maybe windowCount (min windowCount) $ maxIcons cfg
      newImagesNeeded = length existingImages < max (minIcons cfg) maxNeeded
      -- XXX: Only one of the two things being zipped can be an infinite list,
      -- which is why this newImagesNeeded contortion is needed.
      imgSrcs =
        if newImagesNeeded
          then infiniteImages
          else existingImages
      getImgs = maybe imgSrcs (`take` imgSrcs) $ maxIcons cfg
      justWindows = map Just sortedWindows
      windowDatas =
        if newImagesNeeded
          then justWindows ++
               replicate (minIcons cfg - length justWindows) Nothing
          else justWindows ++ repeat Nothing
  newImgs <-
    zipWithM updateIconWidget' getImgs windowDatas
  when newImagesNeeded $ lift $ Gtk.widgetShowAll $ iconsContainer ic
  return newImgs

getWindowStatusString :: WindowData -> T.Text
getWindowStatusString windowData = T.toLower $ T.pack $
  case windowData of
    WindowData { windowMinimized = True } -> "minimized"
    WindowData { windowActive = True } -> show Active
    WindowData { windowUrgent = True } -> show Urgent
    _ -> "normal"

possibleStatusStrings :: [T.Text]
possibleStatusStrings =
  map
    (T.toLower . T.pack)
    [show Active, show Urgent, "minimized", "normal", "inactive"]

updateIconWidget
  :: IconController
  -> IconWidget
  -> Maybe WindowData
  -> WorkspacesIO ()
updateIconWidget _ IconWidget
                   { iconContainer = iconButton
                   , iconWindow = windowRef
                   , iconForceUpdate = updateIcon
                   } windowData = do
  let statusString = maybe "inactive" getWindowStatusString windowData :: T.Text
      setIconWidgetProperties =
        updateWidgetClasses iconButton [statusString] possibleStatusStrings
  void $ updateVar windowRef $ const $ return windowData
  lift $ updateIcon >> setIconWidgetProperties

data WorkspaceButtonController = WorkspaceButtonController
  { button :: Gtk.EventBox
  , buttonWorkspace :: Workspace
  , contentsController :: WWC
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
                (switchOneWorkspace a (length (M.toList workspaces) - 1)) >>
              return True
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
        {button = ebox, buttonWorkspace = workspace, contentsController = cc}

switch :: (MonadIO m) => WorkspacesContext -> WorkspaceIdx -> m Bool
switch ctx idx = do
  liftIO $ flip runReaderT ctx $ liftX11Def () $ switchToWorkspace idx
  return True

instance WorkspaceWidgetController WorkspaceButtonController
  where
    getWidget wbc = lift $ Gtk.toWidget $ button wbc
    updateWidget wbc update = do
      newContents <- updateWidget (contentsController wbc) update
      return wbc { contentsController = newContents }
