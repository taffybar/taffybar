{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.WorkspaceHUD
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------

module System.Taffybar.WorkspaceHUD (
  Context(..),
  ControllerConstructor,
  HUDIO,
  IconInfo(..),
  WWC(..),
  WindowData(..),
  Workspace(..),
  WorkspaceButtonController(..),
  WorkspaceContentsController(..),
  WorkspaceHUDConfig(..),
  WorkspaceState(..),
  WorkspaceUnderlineController(..),
  WorkspaceWidgetController(..),
  IconController(..),
  buildBorderButtonController,
  buildButtonController,
  buildContentsController,
  buildIconController,
  buildLabelController,
  buildPadBox,
  buildUnderlineButtonController,
  buildUnderlineController,
  buildWorkspaceHUD,
  buildWorkspaces,
  defaultBuildContentsController,
  defaultGetIconInfo,
  defaultWorkspaceHUDConfig,
  getWorkspaceToWindows,
  hideEmpty,
  hudFromPagerConfig,
  liftPager,
  liftX11Def,
  setImage,
  widgetSetClass,
  windowTitleClassIconGetter,
) where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Concurrent
import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.RateLimit
import qualified Data.Foldable as F
import           Data.List (intersect, sortBy)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.MultiMap as MM
import qualified Data.Set as Set
import           Data.Time.Units
import           Data.Tuple.Select
import           Data.Tuple.Sequence
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Abstract.Widget as W
import           Graphics.UI.Gtk.General.StyleContext
import qualified Graphics.UI.Gtk.Layout.Table as T
import           Graphics.X11.Xlib.Extras
       hiding (rawGetWindowProperty, getWindowProperty8,
               getWindowProperty16, getWindowProperty32, xSetErrorHandler)
import           Prelude
import           System.Information.EWMHDesktopInfo
import           System.Information.SafeX11
import           System.Information.X11DesktopInfo
import           System.Taffybar.IconImages
import           System.Taffybar.Pager
import           Text.Printf

data WorkspaceState
  = Active
  | Visible
  | Hidden
  | Empty
  | Urgent
  deriving (Show, Eq)

workspaceStates :: [String]
workspaceStates = map show [Active, Visible, Hidden, Empty, Urgent]

data IconInfo
  = IIEWMH EWMHIcon
  | IIFilePath FilePath
  | IIColor ColorRGBA
  | IINone
  deriving (Eq, Show)

transparentInfo :: IconInfo
transparentInfo = IIColor (0, 0, 0, 0)

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

data Context = Context
  { controllersVar :: MV.MVar (M.Map WorkspaceIdx WWC)
  , workspacesVar :: MV.MVar (M.Map WorkspaceIdx Workspace)
  , loggingVar :: MV.MVar Bool
  , hudWidget :: Gtk.HBox
  , hudConfig :: WorkspaceHUDConfig
  , hudPager :: Pager
  }

type HUDIO a = ReaderT Context IO a

liftPager :: PagerIO a -> HUDIO a
liftPager action = asks hudPager >>= lift . runReaderT action

liftX11Def :: a -> X11Property a -> HUDIO a
liftX11Def = (liftPager .) . liftPagerX11Def

widgetSetClass
  :: W.WidgetClass widget
  => widget -> String -> IO ()
widgetSetClass widget klass = do
  context <- Gtk.widgetGetStyleContext widget
  styleContextAddClass context klass

setWorkspaceWidgetStatusClass
  :: W.WidgetClass widget
  => Workspace -> widget -> IO ()
setWorkspaceWidgetStatusClass workspace widget =
  updateWidgetClasses widget [show $ workspaceState workspace] workspaceStates

updateWidgetClasses
  :: W.WidgetClass widget
  => widget -> [String] -> [String] -> IO ()
updateWidgetClasses widget toAdd toRemove = do
  context <- Gtk.widgetGetStyleContext widget
  let hasClass = styleContextHasClass context
      addIfMissing klass =
        hasClass klass >>= (`when` (styleContextAddClass context klass)) . not
      removeIfPresent klass = when (not $ elem klass toAdd) $
        hasClass klass >>= (`when` (styleContextRemoveClass context klass))
  mapM_ removeIfPresent toRemove
  mapM_ addIfMissing toAdd

class WorkspaceWidgetController wc where
  getWidget :: wc -> Gtk.Widget
  updateWidget :: wc -> WidgetUpdate -> HUDIO wc
  updateWidgetX11 :: wc -> WidgetUpdate -> HUDIO wc
  updateWidgetX11 cont _ = return cont

data WWC = forall a. WorkspaceWidgetController a => WWC a

instance WorkspaceWidgetController WWC where
  getWidget (WWC wc) = getWidget wc
  updateWidget (WWC wc) update = WWC <$> updateWidget wc update
  updateWidgetX11 (WWC wc) update = WWC <$> updateWidgetX11 wc update

type ControllerConstructor = Workspace -> HUDIO WWC
type ParentControllerConstructor =
  ControllerConstructor -> ControllerConstructor

data WorkspaceHUDConfig =
  WorkspaceHUDConfig
  { widgetBuilder :: ControllerConstructor
  , widgetGap :: Int
  , windowIconSize :: Int
  , underlineHeight :: Int
  , minWSWidgetSize :: Maybe Int
  , underlinePadding :: Int
  , maxIcons :: Maybe Int
  , minIcons :: Int
  , getIconInfo :: WindowData -> HUDIO IconInfo
  , labelSetter :: Workspace -> HUDIO String
  , updateOnWMIconChange :: Bool
  , showWorkspaceFn :: Workspace -> Bool
  , borderWidth :: Int
  , updateEvents :: [String]
  , updateRateLimitMicroseconds :: Integer
  , debugMode :: Bool
  , iconSort :: [WindowData] -> HUDIO [WindowData]
  , urgentWorkspaceState :: Bool
  }

hudFromPagerConfig :: PagerConfig -> WorkspaceHUDConfig
hudFromPagerConfig pagerConfig =
  let updater workspace
        | any windowUrgent ws = urgentWorkspace pagerConfig name
        | otherwise =
          let getter =
                case state of
                  Urgent -> urgentWorkspace
                  Visible -> visibleWorkspace
                  Active -> activeWorkspace
                  Hidden -> hiddenWorkspace
                  Empty -> emptyWorkspace
          in getter pagerConfig name
        where
          ws = windows workspace
          name = workspaceName workspace
          state = workspaceState workspace
      padded =
        if workspacePad pagerConfig
          then prefixSpace . updater
          else updater
      getCustomImage hasIcon wt wc =
        case customIcon pagerConfig hasIcon wt wc of
          Just fp -> IIFilePath fp
          Nothing -> IINone
  in defaultWorkspaceHUDConfig
     { labelSetter = return . padded
     , minIcons =
         if fillEmptyImages pagerConfig
           then 1
           else 0
     , maxIcons =
         Just $
         if useImages pagerConfig
           then 1
           else 0
     , getIconInfo =
         windowTitleClassIconGetter
           getCustomImage
     , widgetGap = workspaceGap pagerConfig
     , windowIconSize = imageSize pagerConfig
     , widgetBuilder =
         if workspaceBorder pagerConfig
           then buildBorderButtonController
           else buildButtonController defaultBuildContentsController
     , minWSWidgetSize = Nothing
     , iconSort = return
     }
  where
    prefixSpace "" = ""
    prefixSpace s = " " ++ s

windowTitleClassIconGetter
  :: (Bool -> String -> String -> IconInfo)
  -> (WindowData -> HUDIO IconInfo)
windowTitleClassIconGetter customIconF = fn
  where
    fn w@WindowData {windowTitle = wTitle, windowClass = wClass} = do
      ewmhIcon <- defaultGetIconInfo w
      let hasEwmhIcon = ewmhIcon /= IINone
          custIcon = customIconF hasEwmhIcon wTitle wClass
          hasCustomIcon = custIcon /= IINone
      return $ if hasCustomIcon then custIcon else ewmhIcon

defaultWorkspaceHUDConfig :: WorkspaceHUDConfig
defaultWorkspaceHUDConfig =
  WorkspaceHUDConfig
  { widgetBuilder = buildButtonController defaultBuildContentsController
  , widgetGap = 0
  , windowIconSize = 16
  , underlineHeight = 4
  , minWSWidgetSize = Just 30
  , underlinePadding = 1
  , maxIcons = Nothing
  , minIcons = 0
  , getIconInfo = defaultGetIconInfo
  , labelSetter = return . workspaceName
  , updateOnWMIconChange = True
  , showWorkspaceFn = const True
  , borderWidth = 2
  , iconSort = sortWindowsByPosition
  , updateEvents =
      [ "_NET_CURRENT_DESKTOP"
      , "_NET_WM_DESKTOP"
      , "_NET_DESKTOP_NAMES"
      , "_NET_NUMBER_OF_DESKTOPS"
      , "WM_HINTS"
      ]
  , updateRateLimitMicroseconds = 100000
  , debugMode = False
  , urgentWorkspaceState = False
  }

hideEmpty :: Workspace -> Bool
hideEmpty Workspace { workspaceState = Empty } = False
hideEmpty _ = True

hudLogger :: Context -> String -> IO ()
hudLogger ctx txt =
  do
    shouldLog <- MV.readMVar $ loggingVar ctx
    when shouldLog $ putStrLn txt

hudLog :: String -> HUDIO ()
hudLog txt = ask >>= lift . flip hudLogger (printf "[WorkspaceHUD] %s" txt)

updateVar :: MV.MVar a -> (a -> HUDIO a) -> HUDIO a
updateVar var modify = do
  ctx <- ask
  lift $ MV.modifyMVar var $ fmap (\a -> (a, a)) . flip runReaderT ctx . modify

updateWorkspacesVar :: HUDIO (M.Map WorkspaceIdx Workspace)
updateWorkspacesVar = do
  workspacesRef <- asks workspacesVar
  updateVar workspacesRef buildWorkspaces

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

buildWorkspaces :: M.Map WorkspaceIdx Workspace
                -> HUDIO (M.Map WorkspaceIdx Workspace)
buildWorkspaces _ = ask >>= \context -> liftX11Def M.empty $ do
  names <- getWorkspaceNames
  wins <- getWindows
  workspaceToWindows <- getWorkspaceToWindows wins
  urgentWindows <- filterM isWindowUrgent wins
  activeWindows <- readAsListOfWindow Nothing "_NET_ACTIVE_WINDOW"
  active:visible <- getVisibleWorkspaces
  let getWorkspaceState idx ws
        | idx == active = Active
        | idx `elem` visible = Visible
        | urgentWorkspaceState (hudConfig context) &&
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

addWidgetsToTopLevel :: HUDIO ()
addWidgetsToTopLevel = do
  Context { controllersVar = controllersRef
          , hudWidget = cont
          , hudConfig = cfg
          } <- ask
  controllersMap <- lift $ MV.readMVar controllersRef
  -- Elems returns elements in ascending order of their keys so this will always
  -- add the widgets in the correct order
  mapM_ addWidget $ M.elems controllersMap
  when (debugMode cfg) addDebugWidgets
  lift $ Gtk.widgetShowAll cont

addWidget :: WWC -> HUDIO ()
addWidget controller = do
  cont <- asks hudWidget
  let workspaceWidget = getWidget controller
  lift $ do
     -- XXX: This hbox exists to (hopefully) prevent the issue where workspace
     -- widgets appear out of order, in the switcher, by acting as an empty
     -- place holder when the actual widget is hidden.
    hbox <- Gtk.hBoxNew False 0
    parent <- Gtk.widgetGetParent workspaceWidget
    if isJust parent
      then Gtk.widgetReparent (getWidget controller) hbox
      else Gtk.containerAdd hbox workspaceWidget
    Gtk.containerAdd cont hbox

addDebugWidgets :: HUDIO ()
addDebugWidgets = do
  ctx <- ask
  cont <- asks hudWidget
  loggingRef <- asks loggingVar
  let getLabelText state = printf "ToggleLogging: %s" $ show state :: String
  lift $ do
    enableLoggingBox <- Gtk.eventBoxNew
    rebuildBarBox <- Gtk.eventBoxNew
    Gtk.widgetSetName enableLoggingBox "WorkspaceHUD-toggleLogging"
    Gtk.widgetSetName rebuildBarBox "WorkspaceHUD-rebuildButton"
    loggingEnabled <- MV.readMVar loggingRef
    logLabel <- Gtk.labelNew $ Just $ getLabelText loggingEnabled
    rebuildLabel <- Gtk.labelNew $ Just "Rebuild Bar"
    Gtk.widgetSetName logLabel "WorkspaceHUD-toggleLogging"
    Gtk.widgetSetName rebuildLabel "WorkspaceHUD-rebuildButton"
    let toggleLogging =
          MV.modifyMVar_
            loggingRef
            (\current -> do
               let newState = not current
                   labelText = getLabelText newState
               Gtk.labelSetMarkup logLabel labelText
               return $ not current) >>
          return True
        -- Clear the container and repopulate it
        rebuildBar
         = do
          Gtk.containerForeach cont (Gtk.containerRemove cont)
          runReaderT (addWidgetsToTopLevel >> updateAllWorkspaceWidgets) ctx
          return True
    Gtk.containerAdd enableLoggingBox logLabel
    Gtk.containerAdd rebuildBarBox rebuildLabel
    _ <- Gtk.on enableLoggingBox Gtk.buttonPressEvent (liftIO toggleLogging)
    _ <- Gtk.on rebuildBarBox Gtk.buttonPressEvent (liftIO rebuildBar)
    Gtk.containerAdd cont enableLoggingBox
    Gtk.containerAdd cont rebuildBarBox
    return ()

buildWorkspaceHUD :: WorkspaceHUDConfig -> Pager -> IO Gtk.Widget
buildWorkspaceHUD cfg pager = do
  cont <- Gtk.hBoxNew False (widgetGap cfg)
  controllersRef <- MV.newMVar M.empty
  workspacesRef <- MV.newMVar M.empty
  loggingRef <- MV.newMVar False
  let context =
        Context
        { controllersVar = controllersRef
        , workspacesVar = workspacesRef
        , loggingVar = loggingRef
        , hudWidget = cont
        , hudConfig = cfg
        , hudPager = pager
        }
  -- This will actually create all the widgets
  runReaderT updateAllWorkspaceWidgets context
  updateHandler <- onWorkspaceUpdate context
  mapM_ (subscribe pager updateHandler) $ updateEvents cfg
  iconHandler <- onIconsChanged context
  when (updateOnWMIconChange cfg) $
    subscribe pager (onIconChanged context iconHandler) "_NET_WM_ICON"
  return $ Gtk.toWidget cont

updateAllWorkspaceWidgets :: HUDIO ()
updateAllWorkspaceWidgets = do
  hudLog "-Workspace- -Execute-..."

  workspacesMap <- updateWorkspacesVar
  hudLog $ printf "Workspaces: %s" $ show workspacesMap

  hudLog "-Workspace- Adding and removing widgets..."
  updateWorkspaceControllers

  let updateController' idx controller =
        maybe (return controller)
              (updateWidget controller . WorkspaceUpdate) $
              M.lookup idx workspacesMap
      logUpdateController i =
        hudLog $ printf "-Workspace- -each- Updating %s widget" $ show i
      updateController i cont = logUpdateController i >>
                                updateController' i cont

  doWidgetUpdate updateController

  hudLog "-Workspace- Showing and hiding controllers..."
  setControllerWidgetVisibility

setControllerWidgetVisibility :: HUDIO ()
setControllerWidgetVisibility = do
  Context { workspacesVar = workspacesRef
          , controllersVar = controllersRef
          , hudConfig = cfg
          } <- ask
  lift $ do
    workspacesMap <- MV.readMVar workspacesRef
    controllersMap <- MV.readMVar controllersRef
    forM_ (M.elems workspacesMap) $ \ws ->
      let c = M.lookup (workspaceIdx ws) controllersMap
          mWidget = getWidget <$> c
          action = if showWorkspaceFn cfg ws
                   then Gtk.widgetShow
                   else Gtk.widgetHide
      in
        maybe (return ()) action mWidget

doWidgetUpdate :: (WorkspaceIdx -> WWC -> HUDIO WWC) -> HUDIO ()
doWidgetUpdate updateController = do
  c@Context { controllersVar = controllersRef } <- ask
  lift $ MV.modifyMVar_ controllersRef $ \controllers -> do
    controllersList <-
      mapM
      (\(idx, controller) -> do
         newController <- runReaderT (updateController idx controller) c
         return (idx, newController)) $
      M.toList controllers
    return $ M.fromList controllersList

updateWorkspaceControllers :: HUDIO ()
updateWorkspaceControllers = do
  Context { controllersVar = controllersRef
          , workspacesVar = workspacesRef
          , hudWidget = cont
          , hudConfig = cfg
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
     Context
  -> (req -> IO resp)
  -> ResultsCombiner req resp
  -> IO (req -> IO resp)
rateLimitFn context =
  let limit = (updateRateLimitMicroseconds $ hudConfig context)
      rate = fromMicroseconds limit :: Microsecond in
  generateRateLimitedFunction $ PerInvocation rate

onWorkspaceUpdate :: Context -> IO (Event -> IO ())
onWorkspaceUpdate context = do
  rateLimited <- rateLimitFn context doUpdate combineRequests
  let withLog event = do
        case event of
          PropertyEvent _ _ _ _ _ atom _ _ ->
            hudLogger context $ printf "-Event- -Workspace- %s" $ show atom
          _ -> hudLogger context "-Event- -Workspace-"
        void $ forkIO $ rateLimited event
  return withLog
  where
    combineRequests _ b = Just (b, const ((), ()))
    doUpdate _ = Gtk.postGUIAsync $ runReaderT updateAllWorkspaceWidgets context

onIconChanged :: Context -> (Set.Set X11Window -> IO ()) -> Event -> IO ()
onIconChanged context handler event = do
  let logger = hudLogger context
  case event of
    PropertyEvent { ev_window = wid } -> do
      logger $ printf "-Icon- -Event- %s" $ show wid
      handler $ Set.singleton wid
    _ -> return ()

onIconsChanged :: Context -> IO (Set.Set X11Window -> IO ())
onIconsChanged context =
  (.) (void . forkIO) <$> rateLimitFn context onIconsChanged' combineRequests
  where
    combineRequests windows1 windows2 =
      Just (Set.union windows1 windows2, const ((), ()))
    onIconsChanged' wids = do
      hudLogger context $ printf "-Icon- -Execute- %s" $ show wids
      flip runReaderT context $
        doWidgetUpdate
          (\idx c ->
             hudLog (printf "-Icon- -each- Updating %s icons." $ show idx) >>
             updateWidget c (IconUpdate $ Set.toList wids))

data WorkspaceContentsController = WorkspaceContentsController
  { containerWidget :: Gtk.Widget
  , contentsControllers :: [WWC]
  }

buildContentsController :: [ControllerConstructor] -> ControllerConstructor
buildContentsController constructors ws = do
  controllers <- mapM ($ ws) constructors
  tempController <- lift $ do
    cons <- Gtk.hBoxNew False 0
    mapM_ (Gtk.containerAdd cons . getWidget) controllers
    outerBox <- buildPadBox cons
    widgetSetClass cons "Contents"
    return
      WorkspaceContentsController
      { containerWidget = Gtk.toWidget outerBox
      , contentsControllers = controllers
      }
  WWC <$> updateWidget tempController (WorkspaceUpdate ws)

buildPadBox :: W.WidgetClass widget => widget -> IO Gtk.EventBox
buildPadBox cons = do
  innerBox <- Gtk.hBoxNew False 0
  outerBox <- Gtk.eventBoxNew
  Gtk.containerAdd innerBox cons
  Gtk.containerAdd outerBox innerBox
  widgetSetClass innerBox "InnerPad"
  widgetSetClass outerBox "OuterPad"
  return outerBox

defaultBuildContentsController :: ControllerConstructor
defaultBuildContentsController =
  buildContentsController [buildLabelController, buildIconController]

instance WorkspaceWidgetController WorkspaceContentsController where
  getWidget = containerWidget
  updateWidget cc update = do
    Context {hudConfig = cfg} <- ask
    lift $
      maybe (return ()) (updateMinSize $ Gtk.toWidget $ containerWidget cc) $
      minWSWidgetSize cfg
    case update of
      WorkspaceUpdate newWorkspace ->
        lift $ setWorkspaceWidgetStatusClass newWorkspace $ containerWidget cc
      _ -> return ()
    newControllers <- mapM (`updateWidget` update) $ contentsControllers cc
    return cc {contentsControllers = newControllers}
  updateWidgetX11 cc update = do
    newControllers <- mapM (`updateWidgetX11` update) $ contentsControllers cc
    return cc {contentsControllers = newControllers}

data LabelController = LabelController { label :: Gtk.Label }

buildLabelController :: ControllerConstructor
buildLabelController ws = do
  tempController <- lift $ do
    lbl <- Gtk.labelNew (Nothing :: Maybe String)
    widgetSetClass lbl "WorkspaceLabel"
    return LabelController { label = lbl }
  WWC <$> updateWidget tempController (WorkspaceUpdate ws)

instance WorkspaceWidgetController LabelController where
  getWidget = Gtk.toWidget . label
  updateWidget lc (WorkspaceUpdate newWorkspace) = do
    Context { hudConfig = cfg } <- ask
    labelText <- labelSetter cfg newWorkspace
    lift $ do
      Gtk.labelSetMarkup (label lc) labelText
      setWorkspaceWidgetStatusClass newWorkspace $ label lc
    return lc
  updateWidget lc _ = return lc

data IconWidget = IconWidget
  { iconContainer :: Gtk.EventBox
  , iconImage :: Gtk.Image
  , iconWindow :: MV.MVar (Maybe WindowData)
  }

data IconController = IconController
  { iconsContainer :: Gtk.HBox
  , iconImages :: [IconWidget]
  , iconWorkspace :: Workspace
  }

buildIconController :: ControllerConstructor
buildIconController ws = do
  tempController <-
    lift $ do
      hbox <- Gtk.hBoxNew False 0
      return
        IconController
        {iconsContainer = hbox, iconImages = [], iconWorkspace = ws}
  WWC <$> updateWidget tempController (WorkspaceUpdate ws)

instance WorkspaceWidgetController IconController where
  getWidget = Gtk.toWidget . iconsContainer
  updateWidget ic (WorkspaceUpdate newWorkspace) = do
    newImages <- updateImages ic newWorkspace
    return ic { iconImages = newImages, iconWorkspace = newWorkspace }
  updateWidget ic (IconUpdate updatedIcons) =
    updateWindowIconsById ic updatedIcons >> return ic

updateWindowIconsById :: IconController
                      -> [X11Window]
                      -> HUDIO ()
updateWindowIconsById ic windowIds =
  mapM_ maybeUpdateWindowIcon $ iconImages ic
  where
    maybeUpdateWindowIcon widget =
      do
        info <- lift $ MV.readMVar $ iconWindow widget
        when (maybe False (flip elem windowIds . windowId) info) $
         updateIconWidget ic widget info True False

updateMinSize :: Gtk.Widget -> Int  -> IO ()
updateMinSize widget minWidth = do
  W.widgetSetSizeRequest widget (-1) (-1)
  W.Requisition w _ <- W.widgetSizeRequest widget
  when (w < minWidth) $ W.widgetSetSizeRequest widget minWidth  $ -1

defaultGetIconInfo :: WindowData -> HUDIO IconInfo
defaultGetIconInfo w = do
  icons <- liftX11Def [] $ postX11RequestSyncProp (getWindowIcons $ windowId w) []
  iconSize <- asks $ windowIconSize . hudConfig
  return $
    if null icons
      then IINone
      else IIEWMH $ selectEWMHIcon iconSize icons

forkM :: Monad m => (c -> m a) -> (c -> m b) -> c -> m (a, b)
forkM a b = sequenceT . (a &&& b)

sortWindowsByPosition :: [WindowData] -> HUDIO [WindowData]
sortWindowsByPosition wins = do
  let getGeometryHUD w = getDisplay >>= liftIO . (`safeGetGeometry` w)
      getGeometries = mapM
                      (forkM return ((((sel2 &&& sel3) <$>) .) getGeometryHUD) . windowId)
                      wins
  windowGeometries <- liftX11Def [] getGeometries
  let getLeftPos wd =
        fromMaybe (999999999, 99999999) $ lookup (windowId wd) windowGeometries
      compareWindowData a b =
        compare
          (windowMinimized a, getLeftPos a)
          (windowMinimized b, getLeftPos b)
  return $ sortBy compareWindowData wins

updateImages :: IconController -> Workspace -> HUDIO [IconWidget]
updateImages ic ws = do
  Context {hudConfig = cfg} <- ask
  sortedWindows <- iconSort cfg $ windows ws
  let updateIconWidget' getImage wdata ton = do
        let force = isNothing wdata && newImagesNeeded && ton
        iconWidget <- getImage
        _ <- updateIconWidget ic iconWidget wdata force ton
        return iconWidget
      existingImages = map return $ iconImages ic
      buildAndAddIconWidget = do
        iw <- buildIconWidget ws
        lift $ Gtk.containerAdd (iconsContainer ic) $ iconContainer iw
        return iw
      infiniteImages = existingImages ++ repeat buildAndAddIconWidget
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
      transparentOnNones = replicate (minIcons cfg) True ++ repeat False
  newImgs <-
    sequence $ zipWith3 updateIconWidget' getImgs windowDatas transparentOnNones
  when newImagesNeeded $ lift $ Gtk.widgetShowAll $ iconsContainer ic
  return newImgs

buildIconWidget :: Workspace -> HUDIO IconWidget
buildIconWidget ws = do
  ctx <- ask
  lift $ do
    img <- Gtk.imageNew
    ebox <- Gtk.eventBoxNew
    windowVar <- MV.newMVar Nothing
    widgetSetClass img "IconImage"
    widgetSetClass ebox "IconContainer"
    Gtk.containerAdd ebox img
    _ <-
      Gtk.on ebox Gtk.buttonPressEvent $
      liftIO $ do
        info <- MV.readMVar windowVar
        case info of
          Just updatedInfo ->
            flip runReaderT ctx $ liftX11Def () $ focusWindow $ windowId updatedInfo
          _ -> liftIO $ void $ switch ctx (workspaceIdx ws)
        return True
    return
      IconWidget {iconContainer = ebox, iconImage = img, iconWindow = windowVar}

getWindowStatusString :: WindowData -> String
getWindowStatusString WindowData { windowActive = True } = show Active
getWindowStatusString WindowData { windowUrgent = True } = show Urgent
getWindowStatusString WindowData { windowMinimized = True } = "Minimized"
getWindowStatusString _ = "Normal"

possibleStatusStrings :: [String]
possibleStatusStrings = [show Active, show Urgent, "Minimized", "Normal", "Nodata"]

updateIconWidget
  :: IconController
  -> IconWidget
  -> Maybe WindowData
  -> Bool
  -> Bool
  -> HUDIO ()
updateIconWidget _ IconWidget
                   { iconContainer = iconButton
                   , iconImage = image
                   , iconWindow = windowRef
                   } windowData forceUpdate transparentOnNone = do
  cfg <- asks hudConfig

  let setIconWidgetProperties = do
        info <- maybe (return IINone) (getIconInfo cfg) windowData
        let imgSize = windowIconSize cfg
            statusString = maybe "nodata" getWindowStatusString windowData
            iconInfo =
              case info of
                IINone ->
                  if transparentOnNone
                  then transparentInfo
                  else IINone
                _ -> info
        lift $ do
          mpixBuf <- getPixBuf imgSize iconInfo
          setImage imgSize image mpixBuf
          updateWidgetClasses iconButton [statusString] possibleStatusStrings

  void $ updateVar windowRef $ \currentData ->
    when (forceUpdate || (windowId <$> currentData) == (windowId <$> windowData))
         setIconWidgetProperties >> return windowData

setImage :: Int -> Gtk.Image -> Maybe Gtk.Pixbuf -> IO ()
setImage imgSize img pixBuf =
  case pixBuf of
    Just pixbuf -> do
      scaledPixbuf <- scalePixbuf imgSize pixbuf
      Gtk.imageSetFromPixbuf img scaledPixbuf
    Nothing -> Gtk.imageClear img

getPixBuf :: Int -> IconInfo -> IO (Maybe Gtk.Pixbuf)
getPixBuf imgSize = gpb
  where
    gpb (IIEWMH icon) = Just <$> pixBufFromEWMHIcon icon
    gpb (IIFilePath file) = Just <$> pixBufFromFile imgSize file
    gpb (IIColor color) = Just <$> pixBufFromColor imgSize color
    gpb _ = return Nothing

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
  lift $ do
    ebox <- Gtk.eventBoxNew
    Gtk.containerAdd ebox $ getWidget cc
    Gtk.eventBoxSetVisibleWindow ebox False
    _ <-
      Gtk.on ebox Gtk.scrollEvent $ do
        workspaces <- liftIO $ MV.readMVar workspacesRef
        let switchOne a =
              liftIO $
              flip runReaderT ctx $
              liftX11Def
                ()
                (switchOneWorkspace a (length (M.toList workspaces) - 1)) >>
              return True
        dir <- Gtk.eventScrollDirection
        case dir of
          Gtk.ScrollUp -> switchOne True
          Gtk.ScrollLeft -> switchOne True
          Gtk.ScrollDown -> switchOne False
          Gtk.ScrollRight -> switchOne False
          Gtk.ScrollSmooth -> return False
    _ <- Gtk.on ebox Gtk.buttonPressEvent $ switch ctx $ workspaceIdx workspace
    return $
      WWC
        WorkspaceButtonController
        {button = ebox, buttonWorkspace = workspace, contentsController = cc}

switch :: (MonadIO m) => Context -> WorkspaceIdx -> m Bool
switch ctx idx = do
  liftIO $ flip runReaderT ctx $ liftX11Def () $ switchToWorkspace idx
  return True

instance WorkspaceWidgetController WorkspaceButtonController
  where
    getWidget wbc = Gtk.toWidget $ button wbc
    updateWidget wbc update = do
      newContents <- updateWidget (contentsController wbc) update
      return wbc { contentsController = newContents }

data WorkspaceUnderlineController = WorkspaceUnderlineController
  { table :: T.Table
  -- XXX: An event box is used here because we need to change the background
  , underline :: Gtk.EventBox
  , overlineController :: WWC
  }

buildUnderlineController :: ParentControllerConstructor
buildUnderlineController contentsBuilder workspace = do
  cfg <- asks hudConfig
  cc <- contentsBuilder workspace

  lift $ do
    t <- T.tableNew 2 1 False
    u <- Gtk.eventBoxNew
    W.widgetSetSizeRequest u (-1) $ underlineHeight cfg

    T.tableAttach t (getWidget cc) 0 1 0 1
       [T.Expand, T.Fill] [T.Expand, T.Fill] 0 0
    T.tableAttach t u 0 1 1 2
       [T.Fill] [T.Shrink] (underlinePadding cfg) 0

    widgetSetClass u "Underline"
    return $ WWC WorkspaceUnderlineController
      {table = t, underline = u, overlineController = cc}

instance WorkspaceWidgetController WorkspaceUnderlineController where
  getWidget uc = Gtk.toWidget $ table uc
  updateWidget uc wu@(WorkspaceUpdate workspace) =
    lift (setWorkspaceWidgetStatusClass workspace (underline uc)) >>
    updateUnderline uc wu
  updateWidget a b = updateUnderline a b

updateUnderline :: WorkspaceUnderlineController
                -> WidgetUpdate
                -> HUDIO WorkspaceUnderlineController
updateUnderline uc u = do
  newContents <- updateWidget (overlineController uc) u
  return uc { overlineController = newContents }

data WorkspaceBorderController =
  WorkspaceBorderController { border :: Gtk.EventBox
                            , borderContents :: Gtk.EventBox
                            , insideController :: WWC
                            }

buildBorderController :: ParentControllerConstructor
buildBorderController contentsBuilder workspace = do
  cc <- contentsBuilder workspace
  cfg <- asks hudConfig
  lift $ do
    brd <- Gtk.eventBoxNew
    cnt <- Gtk.eventBoxNew
    Gtk.eventBoxSetVisibleWindow brd True
    Gtk.containerSetBorderWidth cnt $ borderWidth cfg
    Gtk.containerAdd brd cnt
    Gtk.containerAdd cnt $ getWidget cc
    widgetSetClass brd "Border"
    widgetSetClass cnt "Container"
    return $
      WWC
        WorkspaceBorderController
        {border = brd, borderContents = cnt, insideController = cc}

instance WorkspaceWidgetController WorkspaceBorderController where
  getWidget bc = Gtk.toWidget $ border bc
  updateWidget bc wu@(WorkspaceUpdate workspace) =
    let setClasses = setWorkspaceWidgetStatusClass workspace
    in lift (setClasses (border bc) >> setClasses (borderContents bc)) >>
       updateBorder bc wu
  updateWidget a b = updateBorder a b

updateBorder :: WorkspaceBorderController
             -> WidgetUpdate
             -> HUDIO WorkspaceBorderController
updateBorder bc update = do
  newContents <- updateWidget (insideController bc) update
  return bc { insideController = newContents }

buildUnderlineButtonController :: ControllerConstructor
buildUnderlineButtonController =
  buildButtonController (buildUnderlineController defaultBuildContentsController)

buildBorderButtonController :: ControllerConstructor
buildBorderButtonController =
  buildButtonController (buildBorderController defaultBuildContentsController)
