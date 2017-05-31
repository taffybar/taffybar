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
  IconInfo(..),
  WWC(..),
  WindowData(..),
  Workspace(..),
  WorkspaceButtonController(..),
  WorkspaceContentsController(..),
  WorkspaceHUDConfig(..),
  WorkspaceUnderlineController(..),
  WorkspaceWidgetController(..),
  buildBorderButtonController,
  buildButtonController,
  buildContentsController,
  buildUnderlineButtonController,
  buildUnderlineController,
  buildWorkspaceHUD,
  buildWorkspaces,
  defaultGetIconInfo,
  defaultWorkspaceHUDConfig,
  getWorkspaceToWindows,
  hideEmpty,
  hudFromPagerConfig,
  windowTitleClassIconGetter
) where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.RateLimit
import qualified Data.Char as S
import qualified Data.Foldable as F
import           Data.List (sortBy)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.MultiMap as MM
import qualified Data.Set as Set
import           Data.Time.Units
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Abstract.Widget as W
import qualified Graphics.UI.Gtk.Layout.Table as T
import           Graphics.X11.Xlib.Extras hiding (xSetErrorHandler)
import           Prelude
import           System.Information.EWMHDesktopInfo
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

data IconInfo
  = IIEWMH EWMHIcon
  | IIFilePath FilePath
  | IIColor ColorRGBA
  | IINone
  deriving (Eq, Show)

transparentInfo :: IconInfo
transparentInfo = IIColor (0, 0, 0, 0)

data WindowData = WindowData { windowId :: X11Window
                             , windowTitle :: String
                             , windowClass :: String
                             , windowUrgent :: Bool
                             , windowActive :: Bool
                             } deriving (Show, Eq)

data WidgetUpdate = WorkspaceUpdate Workspace | IconUpdate [X11Window]

data Workspace = Workspace
  { workspaceIdx :: WorkspaceIdx
  , workspaceName :: String
  , workspaceState :: WorkspaceState
  , windows :: [WindowData]
  } deriving (Show, Eq)

data Context =
  Context { controllersVar :: MV.MVar (M.Map WorkspaceIdx WWC)
          , workspacesVar :: MV.MVar (M.Map WorkspaceIdx Workspace)
          , loggingVar :: MV.MVar Bool
          , hudWidget :: Gtk.HBox
          , hudConfig :: WorkspaceHUDConfig
          , hudPager :: Pager
          }

type HUDIO a = ReaderT Context IO a

liftPager :: PagerIO a -> HUDIO a
liftPager action = asks hudPager >>= lift . runReaderT action

liftX11 :: X11Property a -> HUDIO a
liftX11 = liftPager . liftPagerX11

class WorkspaceWidgetController wc where
  updateWidget :: wc -> WidgetUpdate -> HUDIO wc
  getWidget :: wc -> Gtk.Widget

data WWC = forall a. WorkspaceWidgetController a => WWC a

instance WorkspaceWidgetController WWC where
  getWidget (WWC wc) = getWidget wc
  updateWidget (WWC wc) workspace =
    WWC <$> updateWidget wc workspace

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
  , labelSetter :: Workspace -> String
  , updateIconsOnTitleChange :: Bool
  , updateOnWMIconChange :: Bool
  , showWorkspaceFn :: Workspace -> Bool
  , borderWidth :: Int
  , updateEvents :: [String]
  , updateRateLimitMicroseconds :: Integer
  , debugMode :: Bool
  , sortIcons :: Bool
  , handleX11Errors :: Bool
  , redrawIconsOnStateChange :: Bool
  , urgentWorkspaceState :: Bool
  , innerPadding :: Int
  , outerPadding :: Int
  }

hudFromPagerConfig :: PagerConfig -> WorkspaceHUDConfig
hudFromPagerConfig pagerConfig =
  let updater workspace
        | any windowUrgent ws = urgentWorkspace pagerConfig name
        | otherwise = let getter = case state of
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
      padded = if workspacePad pagerConfig
               then prefixSpace . updater
               else updater
      getCustomImage wt wc = case customIcon pagerConfig wt wc of
                               Just fp -> IIFilePath fp
                               Nothing -> IINone
  in defaultWorkspaceHUDConfig
       { labelSetter = padded
       , minIcons = if fillEmptyImages pagerConfig then 1 else 0
       , maxIcons = Just $ if useImages pagerConfig then 1 else 0
       , getIconInfo = windowTitleClassIconGetter
                       (preferCustomIcon pagerConfig) getCustomImage
       , widgetGap = workspaceGap pagerConfig
       , windowIconSize = imageSize pagerConfig
       , widgetBuilder = if workspaceBorder pagerConfig
                         then
                           buildBorderButtonController
                         else
                           buildButtonController buildContentsController
       , minWSWidgetSize = Nothing
       }
    where
      prefixSpace "" = ""
      prefixSpace s = " " ++ s

defaultWorkspaceHUDConfig :: WorkspaceHUDConfig
defaultWorkspaceHUDConfig =
  WorkspaceHUDConfig
  { widgetBuilder = buildUnderlineButtonController
  , widgetGap = 0
  , windowIconSize = 16
  , underlineHeight = 4
  , minWSWidgetSize = Just 30
  , underlinePadding = 1
  , maxIcons = Nothing
  , minIcons = 0
  , getIconInfo = defaultGetIconInfo
  , labelSetter = workspaceName
  , updateIconsOnTitleChange = True
  , updateOnWMIconChange = True
  , showWorkspaceFn = const True
  , borderWidth = 2
  , sortIcons = True
  , updateEvents =
    [ "_NET_CURRENT_DESKTOP"
    , "_NET_WM_DESKTOP"
    , "_NET_DESKTOP_NAMES"
    , "_NET_NUMBER_OF_DESKTOPS"
    , "WM_HINTS"
    ]
  , updateRateLimitMicroseconds = 100000
  , debugMode = False
  , handleX11Errors = True
  , redrawIconsOnStateChange = False
  , urgentWorkspaceState = False
  , innerPadding = 0
  , outerPadding = 0
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
hudLog txt = ask >>= lift . flip hudLogger txt

updateVar :: MV.MVar a -> (a -> HUDIO a) -> HUDIO a
updateVar var modify = do
  ctx <- ask
  lift $ MV.modifyMVar var $ fmap (\a -> (a, a)) . flip runReaderT ctx . modify

updateWorkspacesVar :: HUDIO (M.Map WorkspaceIdx Workspace)
updateWorkspacesVar = do
  workspacesRef <- asks workspacesVar
  updateVar workspacesRef buildWorkspaces

getWorkspaceToWindows :: HUDIO (MM.MultiMap WorkspaceIdx X11Window)
getWorkspaceToWindows = liftX11 $ getWindows >>=
  foldM
    (\theMap window ->
       MM.insert <$> getWorkspace window
                 <*> pure window <*> pure theMap)
    MM.empty

getUrgentWindows :: X11Property [X11Window]
getUrgentWindows = getWindows >>= filterM isWindowUrgent

getWindowData :: [X11Window] -> [X11Window] -> X11Window -> X11Property WindowData
getWindowData activeWindows urgentWindows window = do
  wTitle <- getWindowTitle window
  wClass <- getWindowClass window
  return
    WindowData
    { windowId = window
    , windowTitle = wTitle
    , windowClass = wClass
    , windowUrgent = window `elem` urgentWindows
    , windowActive = window `elem` activeWindows
    }

buildWorkspaces :: M.Map WorkspaceIdx Workspace
                -> HUDIO (M.Map WorkspaceIdx Workspace)
buildWorkspaces _ = do
  context <- ask

  names <- liftX11 getWorkspaceNames
  workspaceToWindows <- getWorkspaceToWindows
  urgentWindows <- liftX11 getUrgentWindows
  activeWindows <- liftX11 $ readAsListOfWindow Nothing "_NET_ACTIVE_WINDOW"
  active:visible <- liftX11 getVisibleWorkspaces
  let
    getWorkspaceState idx ws
        | urgentWorkspaceState (hudConfig context) &&
          not (null urgentWindows) = Urgent
        | idx == active = Active
        | idx `elem` visible = Visible
        | null ws = Empty
        | otherwise = Hidden

  foldM (\theMap (idx, name) ->
           do
             let ws = MM.lookup idx workspaceToWindows
             windowInfos <- liftX11 $ mapM (getWindowData activeWindows urgentWindows) ws
             return $ M.insert idx
                    Workspace { workspaceIdx = idx
                              , workspaceName = name
                              , workspaceState = getWorkspaceState idx ws
                              , windows = windowInfos
                              } theMap) M.empty names

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
  let context = Context { controllersVar = controllersRef
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
  showControllers

showControllers :: HUDIO ()
showControllers = do
  Context { workspacesVar = workspacesRef
          , controllersVar = controllersRef
          , hudConfig = cfg
          } <- ask
  workspacesMap <- lift $ MV.readMVar workspacesRef
  controllersMap <- lift $ MV.readMVar controllersRef
  forM_ (M.elems workspacesMap) $ \ws ->
    let c = M.lookup (workspaceIdx ws) controllersMap
        mWidget = getWidget <$> c
        action = lift . if showWorkspaceFn cfg ws
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

data IconWidget = IconWidget { iconContainer :: Gtk.EventBox
                             , iconImage :: Gtk.Image
                             , iconWindow :: MV.MVar (Maybe WindowData)
                             }

data WorkspaceContentsController = WorkspaceContentsController
  { containerEbox :: Gtk.EventBox
  , containerWidget :: Gtk.Widget
  , container :: Gtk.HBox
  , label :: Gtk.Label
  , iconImages :: [IconWidget]
  , contentsWorkspace :: Workspace
  }

buildContentsController :: ControllerConstructor
buildContentsController ws = do
  context <- ask
  tempController <- lift $ do
    lbl <- Gtk.labelNew (Nothing :: Maybe String)
    hbox <- Gtk.hBoxNew False 0
    ebox <- Gtk.eventBoxNew
    Gtk.containerAdd hbox lbl
    ial <- Gtk.alignmentNew 0.5 0.5 0 0
    oal <- Gtk.alignmentNew 0.5 0.5 1 1
    let ipadding = innerPadding $ hudConfig context
        opadding = outerPadding $ hudConfig context
    Gtk.alignmentSetPadding ial ipadding ipadding ipadding ipadding
    Gtk.alignmentSetPadding oal opadding opadding opadding opadding
    Gtk.containerAdd ial hbox
    Gtk.containerAdd ebox ial
    Gtk.containerAdd oal ebox
    return
      WorkspaceContentsController
      { containerEbox = ebox
      , container = hbox
      , containerWidget = Gtk.toWidget oal
      , label = lbl
      , iconImages = []
      , contentsWorkspace =
          ws {windows = [], workspaceName = workspaceName ws ++ "fake"}
      }
  WWC <$> updateWidget tempController (WorkspaceUpdate ws)

instance WorkspaceWidgetController WorkspaceContentsController where
  getWidget = containerWidget
  updateWidget cc (WorkspaceUpdate newWorkspace) = do
    Context { hudConfig = cfg } <- ask
    lift $ do
      let currentWorkspace = contentsWorkspace cc
          getLabel = labelSetter cfg

      when (getLabel currentWorkspace /= getLabel newWorkspace) $
           Gtk.labelSetMarkup (label cc) (getLabel newWorkspace)

      setContainerWidgetNames cc newWorkspace

      maybe (return ()) (updateMinSize $ Gtk.toWidget $ container cc) $
            minWSWidgetSize cfg

    let previousState = workspaceState $ contentsWorkspace cc
        stateChanged = previousState /= workspaceState newWorkspace
        redrawForStateChange = redrawIconsOnStateChange cfg && stateChanged

    newImages <- updateImages cc newWorkspace

    when redrawForStateChange $ lift $ Gtk.widgetQueueDraw $ containerEbox cc

    return cc { contentsWorkspace = newWorkspace
              , iconImages = newImages
              }
  updateWidget cc (IconUpdate updatedIcons) =
    updateWindowIconsById cc updatedIcons >> return cc

updateWindowIconsById :: WorkspaceContentsController
                      -> [X11Window]
                      -> HUDIO ()
updateWindowIconsById wcc windowIds =
  mapM_ maybeUpdateWindowIcon $ iconImages wcc
  where
    maybeUpdateWindowIcon widget =
      do
        info <- lift $ MV.readMVar $ iconWindow widget
        when (maybe False (flip elem windowIds . windowId) info) $
         updateIconWidget wcc widget info True False

setContainerWidgetNames :: WorkspaceContentsController -> Workspace -> IO ()
setContainerWidgetNames wcc ws = do
  let getWName = getWidgetName ws
      contentsName = getWName "contents"
      labelName = getWName "label"
  Gtk.widgetSetName (containerEbox wcc) contentsName
  Gtk.widgetSetName (label wcc) labelName

updateMinSize :: Gtk.Widget -> Int  -> IO ()
updateMinSize widget minWidth = do
  W.widgetSetSizeRequest widget (-1) (-1)
  W.Requisition w _ <- W.widgetSizeRequest widget
  when (w < minWidth) $ W.widgetSetSizeRequest widget minWidth  $ -1

defaultGetIconInfo :: WindowData -> HUDIO IconInfo
defaultGetIconInfo w = do
  icons <- liftX11 $ getWindowIcons $ windowId w
  iconSize <- asks $ windowIconSize .hudConfig
  return $
    if null icons
      then IINone
      else IIEWMH $ selectEWMHIcon iconSize icons

windowTitleClassIconGetter
  :: Bool
  -> (String -> String -> IconInfo)
  -> (WindowData -> HUDIO IconInfo)
windowTitleClassIconGetter preferCustom customIconF = fn
  where
    fn w@WindowData {windowTitle = wTitle, windowClass = wClass} = do
      let customResult = customIconF wTitle wClass
      defaultResult <- defaultGetIconInfo w
      let first =
            if preferCustom
              then customResult
              else defaultResult
      let second =
            if preferCustom
              then defaultResult
              else customResult
      return $
        case first of
          IINone -> second
          _ -> first

updateImages :: WorkspaceContentsController -> Workspace -> HUDIO [IconWidget]
updateImages wcc ws = do
  Context {hudConfig = cfg} <- ask
  let updateIconWidget' getImage wdata ton = do
        let forceHack = isNothing wdata && newImagesNeeded && ton
            previousState = workspaceState $ contentsWorkspace wcc
            stateChanged = previousState /= workspaceState ws
            forceForStateChange = redrawIconsOnStateChange cfg && stateChanged
            force = forceHack || forceForStateChange
        iconWidget <- getImage
        _ <- updateIconWidget wcc iconWidget wdata force ton
        return iconWidget
      existingImages = map return $ iconImages wcc
      infiniteImages =
        existingImages ++
        repeat
          (do iw <- buildIconWidget ws
              lift $ Gtk.containerAdd (container wcc) $ iconContainer iw
              return iw)
      windowCount = length $ windows ws
      maxNeeded = maybe windowCount (min windowCount) $ maxIcons cfg
      newImagesNeeded = length existingImages < max (minIcons cfg) maxNeeded
      imgSrcs =
        if newImagesNeeded
          then infiniteImages
          else existingImages
      getImgs =
        case maxIcons cfg of
          Just theMax -> take theMax imgSrcs
          Nothing -> imgSrcs
  -- XXX: Only one of the two things being zipped can be an infinite list, which
  -- is why this newImagesNeeded contortion is needed.
  let makeComparisonTuple wd = (windowClass wd, windowId wd)
      compareWindowData a b = compare (makeComparisonTuple a) (makeComparisonTuple b)
      sortedWindows = if sortIcons cfg then
                        sortBy compareWindowData $ windows ws
                      else
                        windows ws
      justWindows = map Just sortedWindows
      windowDatas =
        if newImagesNeeded
          then justWindows ++
               replicate (minIcons cfg - length justWindows) Nothing
          else justWindows ++ repeat Nothing
      transparentOnNones = replicate (minIcons cfg) True ++ repeat False
  newImgs <-
    sequence $ zipWith3 updateIconWidget' getImgs windowDatas transparentOnNones
  when newImagesNeeded $ lift $ Gtk.widgetShowAll $ container wcc
  return newImgs

buildIconWidget :: Workspace -> HUDIO IconWidget
buildIconWidget ws = do
  ctx <- ask
  lift $ do
    img <- Gtk.imageNew
    ebox <- Gtk.eventBoxNew
    windowVar <- MV.newMVar Nothing
    Gtk.containerAdd ebox img
    _ <-
      Gtk.on ebox Gtk.buttonPressEvent $
      liftIO $ do
        info <- MV.readMVar windowVar
        case info of
          Just updatedInfo ->
            flip runReaderT ctx $ liftX11 $ focusWindow $ windowId updatedInfo
          _ -> liftIO $ void $ switch ctx (workspaceIdx ws)
        return True
    return
      IconWidget {iconContainer = ebox, iconImage = img, iconWindow = windowVar}

updateIconWidget
  :: WorkspaceContentsController
  -> IconWidget
  -> Maybe WindowData
  -> Bool
  -> Bool
  -> HUDIO ()
updateIconWidget _ IconWidget { iconContainer = iconButton
                              , iconImage = image
                              , iconWindow = windowRef
                              } windowData forceUpdate transparentOnNone = do
  cfg <- asks hudConfig
  void $ updateVar windowRef $ \currentData ->
    let requireFullEqualityForSkip = updateIconsOnTitleChange cfg
        sameWindow = (windowId <$> currentData) == (windowId <$> windowData)
        dataRequiresUpdate =
          (requireFullEqualityForSkip && (currentData /= windowData)) ||
          not sameWindow
    in when (forceUpdate || dataRequiresUpdate) setIconWidgetProperties >>
       return windowData
  where
    setIconWidgetProperties = do
      cfg <- asks hudConfig
      info <-
        case windowData of
          Just dat -> getIconInfo cfg dat
          Nothing -> return IINone
      let imgSize = windowIconSize cfg
          statusStr =
            case windowData of
              Just WindowData { windowActive = True } -> "active"
              Just WindowData { windowUrgent = True } -> "urgent"
              _ -> "normal"
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
        let widgetName =
              printf
              "Workspace-icon-%s-%s"
              (show $ maybe 0 windowId windowData)
              statusStr
        Gtk.widgetSetName iconButton (widgetName :: String)

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

data WorkspaceButtonController =
  WorkspaceButtonController { button :: Gtk.EventBox
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
              liftIO $ flip runReaderT ctx $ liftX11 $
              switchOneWorkspace a (length (M.toList workspaces) - 1) >>
              return True
        dir <- Gtk.eventScrollDirection
        case dir of
          Gtk.ScrollUp -> switchOne True
          Gtk.ScrollLeft -> switchOne True
          Gtk.ScrollDown -> switchOne False
          Gtk.ScrollRight -> switchOne False
    _ <- Gtk.on ebox Gtk.buttonPressEvent $ switch ctx $ workspaceIdx workspace
    return $
           WWC
           WorkspaceButtonController
           {button = ebox, buttonWorkspace = workspace, contentsController = cc}

switch :: (MonadIO m) => Context -> WorkspaceIdx -> m Bool
switch ctx idx = do
  liftIO $ flip runReaderT ctx $ liftX11 $ switchToWorkspace idx
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

    return $ WWC WorkspaceUnderlineController
      {table = t, underline = u, overlineController = cc}

instance WorkspaceWidgetController WorkspaceUnderlineController where
  getWidget uc = Gtk.toWidget $ table uc
  updateWidget uc wu@(WorkspaceUpdate workspace) =
    let widgetName = getWidgetName workspace "underline"
        setWidgetName = Gtk.widgetSetName (underline uc) widgetName
    in lift setWidgetName >> updateUnderline uc wu
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
    return $
      WWC
        WorkspaceBorderController
        {border = brd, borderContents = cnt, insideController = cc}

instance WorkspaceWidgetController WorkspaceBorderController where
  getWidget bc = Gtk.toWidget $ border bc
  updateWidget bc wu@(WorkspaceUpdate workspace) =
    let setBorderName = Gtk.widgetSetName (border bc) $
                        getWidgetName workspace "Border"
        setContentsName = Gtk.widgetSetName (borderContents bc) $
                          getWidgetName workspace "Container"
    in lift (setBorderName >> setContentsName) >> updateBorder bc wu
  updateWidget a b = updateBorder a b

updateBorder :: WorkspaceBorderController
             -> WidgetUpdate
             -> HUDIO WorkspaceBorderController
updateBorder bc update = do
  newContents <- updateWidget (insideController bc) update
  return bc { insideController = newContents }

getWidgetName :: Workspace -> String -> String
getWidgetName ws wname =
  printf
    "Workspace-%s-%s-%s"
    wname
    (workspaceName ws)
    (map S.toLower $ show $ workspaceState ws)

buildUnderlineButtonController :: ControllerConstructor
buildUnderlineButtonController =
  buildButtonController (buildUnderlineController buildContentsController)

buildBorderButtonController :: ControllerConstructor
buildBorderButtonController =
  buildButtonController (buildBorderController buildContentsController)
