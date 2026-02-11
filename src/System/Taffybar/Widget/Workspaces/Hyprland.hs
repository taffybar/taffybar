{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Workspaces.Hyprland
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Hyprland workspaces widget backed by hyprctl.
module System.Taffybar.Widget.Workspaces.Hyprland where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM.TChan (TChan, readTChan)
import Control.Monad (foldM, forM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Default (Default (..))
import qualified Data.Foldable as F
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (intercalate, sortOn, stripPrefix)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.MultiMap as MM
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import System.Environment.XDG.DesktopEntry
  ( DesktopEntry,
    deFilename,
    getDirectoryEntriesDefault,
  )
import System.Log.Logger (Priority (..), logM)
import System.Taffybar.Context
import System.Taffybar.Hyprland
  ( getHyprlandEventChan,
    runHyprlandCommandJsonT,
    runHyprlandCommandRawT,
  )
import qualified System.Taffybar.Information.Hyprland as Hypr
import System.Taffybar.Util
import System.Taffybar.Widget.Generic.ScalingImage (getScalingImageStrategy)
import System.Taffybar.Widget.Util
  ( WindowIconWidget (..),
    computeIconStripLayout,
    getImageForDesktopEntry,
    handlePixbufGetterException,
    scaledPixbufGetter,
    syncWidgetPool,
    updateWindowIconWidgetState,
    widgetSetClassGI,
    windowStatusClassFromFlags,
  )
import System.Taffybar.Widget.Workspaces.Config
import System.Taffybar.Widget.Workspaces.Shared
  ( WorkspaceState (..),
    buildWorkspaceIconLabelOverlay,
    mkWorkspaceIconWidget,
    setWorkspaceWidgetStatusClass,
  )
import System.Taffybar.WindowIcon (getWindowIconFromClasses, pixBufFromColor)
import Text.Printf (printf)

stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix suffix value =
  reverse <$> stripPrefix (reverse suffix) (reverse value)

isSpecialHyprWorkspace :: HyprlandWorkspace -> Bool
isSpecialHyprWorkspace ws =
  let name = T.toLower $ T.pack $ workspaceName ws
   in T.isPrefixOf "special" name || workspaceIdx ws < 0

data HyprlandWindow = HyprlandWindow
  { windowAddress :: Text,
    windowTitle :: String,
    windowClass :: Maybe String,
    windowInitialClass :: Maybe String,
    -- | The top-left position (x, y) of the window, as reported by
    -- @hyprctl clients -j@. This is used for optional icon ordering.
    windowAt :: Maybe (Int, Int),
    windowUrgent :: Bool,
    windowActive :: Bool,
    windowMinimized :: Bool
  }
  deriving (Show, Eq)

data HyprlandWorkspace = HyprlandWorkspace
  { workspaceIdx :: Int,
    workspaceName :: String,
    workspaceState :: WorkspaceState,
    windows :: [HyprlandWindow]
  }
  deriving (Show, Eq)

newtype HyprlandWorkspaceCache = HyprlandWorkspaceCache [HyprlandWorkspace]

newtype HyprlandWorkspaceWidgetCache
  = HyprlandWorkspaceWidgetCache (M.Map Int HyprlandWorkspaceEntry)

newtype HyprlandWorkspaceOrderCache
  = HyprlandWorkspaceOrderCache [Int]

data HyprlandWorkspaceEntry = HyprlandWorkspaceEntry
  { hweWrapper :: Gtk.Widget,
    hweController :: HyprlandWWC,
    hweLast :: HyprlandWorkspace
  }

type HyprlandIconWidget = WindowIconWidget HyprlandWindow

-- | Controller typeclass for Hyprland workspace widgets, mirroring
-- the X11 'WorkspaceWidgetController' pattern.
class HyprlandWorkspaceWidgetController wc where
  hwcGetWidget :: wc -> IO Gtk.Widget
  hwcUpdateWidget :: wc -> HyprlandWorkspace -> TaffyIO wc

-- | Existential wrapper for Hyprland workspace controllers.
data HyprlandWWC = forall a. (HyprlandWorkspaceWidgetController a) => HyprlandWWC a

instance HyprlandWorkspaceWidgetController HyprlandWWC where
  hwcGetWidget (HyprlandWWC wc) = hwcGetWidget wc
  hwcUpdateWidget (HyprlandWWC wc) ws = HyprlandWWC <$> hwcUpdateWidget wc ws

type HyprlandControllerConstructor = HyprlandWorkspace -> TaffyIO HyprlandWWC

type HyprlandParentControllerConstructor =
  HyprlandControllerConstructor -> HyprlandControllerConstructor

type HyprlandWindowIconPixbufGetter =
  Int32 -> HyprlandWindow -> TaffyIO (Maybe Gdk.Pixbuf)

data HyprlandWorkspacesConfig
  = HyprlandWorkspacesConfig
  { getWorkspaces :: TaffyIO [HyprlandWorkspace],
    switchToWorkspace :: HyprlandWorkspace -> TaffyIO (),
    updateIntervalSeconds :: Double,
    widgetBuilder :: HyprlandControllerConstructor,
    widgetGap :: Int,
    maxIcons :: Maybe Int,
    minIcons :: Int,
    iconSize :: Int32,
    getWindowIconPixbuf :: HyprlandWindowIconPixbufGetter,
    labelSetter :: HyprlandWorkspace -> TaffyIO String,
    showWorkspaceFn :: HyprlandWorkspace -> Bool,
    iconSort :: [HyprlandWindow] -> TaffyIO [HyprlandWindow],
    urgentWorkspaceState :: Bool
  }

hyprlandWorkspacesCommonConfig ::
  HyprlandWorkspacesConfig ->
  WorkspaceWidgetCommonConfig (ReaderT Context IO) HyprlandWorkspace HyprlandWindow HyprlandWWC
hyprlandWorkspacesCommonConfig cfg =
  WorkspaceWidgetCommonConfig
    { commonWidgetBuilder = widgetBuilder cfg,
      commonWidgetGap = widgetGap cfg,
      commonMaxIcons = maxIcons cfg,
      commonMinIcons = minIcons cfg,
      commonGetWindowIconPixbuf = getWindowIconPixbuf cfg,
      commonLabelSetter = labelSetter cfg,
      commonShowWorkspaceFn = showWorkspaceFn cfg,
      commonIconSort = iconSort cfg,
      commonUrgentWorkspaceState = urgentWorkspaceState cfg
    }

applyCommonHyprlandWorkspacesConfig ::
  WorkspaceWidgetCommonConfig (ReaderT Context IO) HyprlandWorkspace HyprlandWindow HyprlandWWC ->
  HyprlandWorkspacesConfig ->
  HyprlandWorkspacesConfig
applyCommonHyprlandWorkspacesConfig common cfg =
  cfg
    { widgetBuilder = commonWidgetBuilder common,
      widgetGap = commonWidgetGap common,
      maxIcons = commonMaxIcons common,
      minIcons = commonMinIcons common,
      getWindowIconPixbuf = commonGetWindowIconPixbuf common,
      labelSetter = commonLabelSetter common,
      showWorkspaceFn = commonShowWorkspaceFn common,
      iconSort = commonIconSort common,
      urgentWorkspaceState = commonUrgentWorkspaceState common
    }

defaultHyprlandWorkspacesConfig :: HyprlandWorkspacesConfig
defaultHyprlandWorkspacesConfig = cfg
  where
    cfg =
      HyprlandWorkspacesConfig
        { getWorkspaces = getHyprlandWorkspaces,
          switchToWorkspace = hyprlandSwitchToWorkspace,
          updateIntervalSeconds = 1,
          widgetBuilder = defaultHyprlandWidgetBuilder cfg,
          widgetGap = 0,
          maxIcons = Nothing,
          minIcons = 0,
          iconSize = 16,
          getWindowIconPixbuf = defaultHyprlandGetWindowIconPixbuf,
          labelSetter = return . workspaceName,
          showWorkspaceFn = \ws ->
            workspaceState ws /= Empty && not (isSpecialHyprWorkspace ws),
          -- Match the X11 Workspaces widget default: order icons by window position.
          iconSort = pure . sortHyprlandWindowsByPosition,
          urgentWorkspaceState = False
        }

instance Default HyprlandWorkspacesConfig where
  def = defaultHyprlandWorkspacesConfig

hyprlandWorkspacesNew :: HyprlandWorkspacesConfig -> TaffyIO Gtk.Widget
hyprlandWorkspacesNew cfg = do
  let common = hyprlandWorkspacesCommonConfig cfg
  cont <-
    liftIO $
      Gtk.boxNew Gtk.OrientationHorizontal $
        fromIntegral (commonWidgetGap common)
  _ <- widgetSetClassGI cont "workspaces"
  ctx <- ask
  let refresh = runReaderT (refreshWorkspaces cfg cont) ctx
  liftIO refresh
  -- Ensure this top-level container is visible when packed into the bar.
  -- Otherwise the start widget area can appear blank under Wayland/Hyprland.
  liftIO $ Gtk.widgetShowAll cont
  eventChan <- getHyprlandEventChan
  events <- liftIO $ Hypr.subscribeHyprlandEvents eventChan
  tid <- liftIO $ forkIO $ hyprlandUpdateLoop refresh events
  _ <- liftIO $ Gtk.onWidgetUnrealize cont $ killThread tid
  Gtk.toWidget cont

hyprlandUpdateLoop :: IO () -> TChan T.Text -> IO ()
hyprlandUpdateLoop refresh events = do
  line <- atomically $ readTChan events
  when (isRelevantHyprEvent (T.unpack line)) refresh
  hyprlandUpdateLoop refresh events

isRelevantHyprEvent :: String -> Bool
isRelevantHyprEvent line =
  let eventName = takeWhile (/= '>') line
   in eventName
        `elem` [ "workspace",
                 "workspacev2",
                 "focusedmon",
                 "activewindow",
                 "activewindowv2",
                 "openwindow",
                 "closewindow",
                 "movewindow",
                 "movewindowv2",
                 "moveworkspace",
                 "renameworkspace",
                 "createworkspace",
                 "destroyworkspace",
                 "monitoradded",
                 "monitorremoved",
                 -- Synthetic "event" emitted by our event reader thread whenever it
                 -- (re)connects to Hyprland. This lets us refresh after a compositor
                 -- restart without polling.
                 "taffybar-hyprland-connected"
               ]

refreshWorkspaces :: HyprlandWorkspacesConfig -> Gtk.Box -> ReaderT Context IO ()
refreshWorkspaces cfg cont = do
  let common = hyprlandWorkspacesCommonConfig cfg
  ws <- getWorkspaces cfg
  HyprlandWorkspaceCache prev <- getStateDefault $ return (HyprlandWorkspaceCache [])
  -- Detect whether the cached widgets have been orphaned (e.g. the bar window
  -- was destroyed and recreated after a compositor reload).  When the old GTK
  -- objects are gone we must re-render even if the workspace data is unchanged.
  HyprlandWorkspaceWidgetCache wc <-
    getStateDefault $ return (HyprlandWorkspaceWidgetCache M.empty)
  widgetsStale <- case M.elems wc of
    [] -> return False
    (first : _) -> liftIO $ not <$> Gtk.widgetGetRealized (hweWrapper first)
  let ignoreEmptyResult = null ws && not (null prev)
  when ignoreEmptyResult $
    liftIO $
      wLog WARNING $
        printf
          "Hyprland workspaces refresh returned empty list; retaining previous state (prevTotal=%d)."
          (length prev)
  let needsRender = (ws /= prev || widgetsStale) && not ignoreEmptyResult
  when needsRender $ do
    liftIO $
      wLog DEBUG $
        printf
          "Hyprland workspaces refresh: total=%d shown=%d (minIcons=%d maxIcons=%s) widgetsStale=%s %s"
          (length ws)
          (length (filter (commonShowWorkspaceFn common) ws))
          (commonMinIcons common)
          (show (commonMaxIcons common))
          (show widgetsStale)
          (summarizeWorkspaces ws)
    _ <- setState (HyprlandWorkspaceCache ws)
    ctx <- ask
    liftIO $ postGUIASync $ runReaderT (renderWorkspaces cfg cont ws) ctx

renderWorkspaces ::
  HyprlandWorkspacesConfig -> Gtk.Box -> [HyprlandWorkspace] -> ReaderT Context IO ()
renderWorkspaces cfg cont workspaces = do
  let common = hyprlandWorkspacesCommonConfig cfg
  let workspaces' = map (applyUrgentState cfg) workspaces
  HyprlandWorkspaceWidgetCache widgetCache <-
    getStateDefault $ return (HyprlandWorkspaceWidgetCache M.empty)
  HyprlandWorkspaceOrderCache prevOrder <-
    getStateDefault $ return (HyprlandWorkspaceOrderCache [])
  -- If any cached widget has been unrealized (e.g. because the bar window was
  -- recreated after a compositor reload), the old GTK objects are dead and must
  -- be discarded so that fresh widgets are built for the new container.
  stale <- case M.elems widgetCache of
    [] -> return False
    (first : _) -> liftIO $ not <$> Gtk.widgetGetRealized (hweWrapper first)
  let oldCache = if stale then M.empty else widgetCache
      oldOrder = if stale then [] else prevOrder
  when stale $
    liftIO $
      wLog DEBUG "renderWorkspaces: discarding stale widget cache (widgets unrealized)"
  let buildOrUpdate newCache ws = do
        let idx = workspaceIdx ws
        entry <- case M.lookup idx oldCache of
          Just prevEntry
            | hweLast prevEntry == ws -> return prevEntry
            | otherwise -> do
                newCtrl <- hwcUpdateWidget (hweController prevEntry) ws
                return prevEntry {hweController = newCtrl, hweLast = ws}
          Nothing -> do
            ctrl <- commonWidgetBuilder common ws
            ctrlWidget <- liftIO $ hwcGetWidget ctrl
            wrapperBox <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0
            liftIO $ Gtk.containerAdd wrapperBox ctrlWidget
            wrapper <- Gtk.toWidget wrapperBox
            return
              HyprlandWorkspaceEntry
                { hweWrapper = wrapper,
                  hweController = ctrl,
                  hweLast = ws
                }
        return (M.insert idx entry newCache, entry)

  (newCache, orderedEntriesRev) <-
    foldM
      ( \(cacheAcc, entriesAcc) ws -> do
          (cacheAcc', entry) <- buildOrUpdate cacheAcc ws
          return (cacheAcc', (ws, entry) : entriesAcc)
      )
      (M.empty, [])
      workspaces'
  let orderedEntries = reverse orderedEntriesRev

  let primaryShowFn = commonShowWorkspaceFn common
      primaryShownCount = length $ filter (primaryShowFn . fst) orderedEntries
      fallbackShowFn ws = workspaceState ws /= Empty
      fallbackShownCount = length $ filter (fallbackShowFn . fst) orderedEntries
      (finalShowFn, finalShownCount, fallbackUsed) =
        if primaryShownCount == 0 && fallbackShownCount > 0
          then (fallbackShowFn, fallbackShownCount, True)
          else (primaryShowFn, primaryShownCount, False)

  when (fallbackUsed && not (null orderedEntries)) $
    liftIO $
      wLog WARNING $
        printf
          "Hyprland workspaces: showWorkspaceFn hid all %d workspaces; falling back to showing non-empty workspaces (including special:*). %s"
          (length orderedEntries)
          (summarizeWorkspaces (map fst orderedEntries))
  when (finalShownCount == 0 && not (null orderedEntries)) $
    liftIO $
      wLog WARNING $
        printf
          "Hyprland workspaces widget is blank: no workspaces passed the show filter (total=%d). %s"
          (length orderedEntries)
          (summarizeWorkspaces (map fst orderedEntries))

  -- Remove wrappers for workspaces that disappeared.
  let removed = M.difference oldCache newCache
  forM_ (M.elems removed) $ \entry ->
    liftIO $ Gtk.containerRemove cont (hweWrapper entry)

  -- Add wrappers for newly created workspaces.
  let added = M.difference newCache oldCache
  forM_ (M.elems added) $ \entry -> do
    liftIO $ Gtk.containerAdd cont (hweWrapper entry)
    liftIO $ Gtk.widgetShowAll (hweWrapper entry)

  let desiredOrder = map (workspaceIdx . fst) orderedEntries
      needsReorder =
        desiredOrder /= oldOrder || not (M.null added) || not (M.null removed)
  when needsReorder $ do
    -- Reorder wrappers to match the order returned by hyprctl.
    forM_ (zip [0 :: Int ..] orderedEntries) $ \(pos, (_ws, entry)) ->
      liftIO $ Gtk.boxReorderChild cont (hweWrapper entry) (fromIntegral pos)

  -- Show/hide the controller widget without removing the wrapper from the box.
  forM_ orderedEntries $ \(ws, entry) -> do
    ctrlWidget <- liftIO $ hwcGetWidget (hweController entry)
    if finalShowFn ws
      then liftIO $ Gtk.widgetShow ctrlWidget
      else liftIO $ Gtk.widgetHide ctrlWidget

  _ <- setState (HyprlandWorkspaceWidgetCache newCache)
  _ <- setState (HyprlandWorkspaceOrderCache desiredOrder)
  return ()

summarizeWorkspaces :: [HyprlandWorkspace] -> String
summarizeWorkspaces wss =
  let summarizeOne ws =
        printf
          "%d:%s:%s(wins=%d)"
          (workspaceIdx ws)
          (workspaceName ws)
          (show (workspaceState ws))
          (length (windows ws))
      maxShown = 12
      body = intercalate ", " $ map summarizeOne (take maxShown wss)
      suffix = if length wss > maxShown then ", ..." else ""
   in "workspaces=[" ++ body ++ suffix ++ "]"

applyUrgentState :: HyprlandWorkspacesConfig -> HyprlandWorkspace -> HyprlandWorkspace
applyUrgentState cfg ws
  | commonUrgentWorkspaceState (hyprlandWorkspacesCommonConfig cfg)
      && workspaceState ws == Hidden
      && any windowUrgent (windows ws) =
      ws {workspaceState = Urgent}
  | otherwise = ws

-- Controller types

data HyprlandLabelController = HyprlandLabelController
  { hlcLabel :: Gtk.Label,
    hlcLabelSetter :: HyprlandWorkspace -> TaffyIO String
  }

instance HyprlandWorkspaceWidgetController HyprlandLabelController where
  hwcGetWidget = Gtk.toWidget . hlcLabel
  hwcUpdateWidget lc ws = do
    labelText <- hlcLabelSetter lc ws
    liftIO $ do
      Gtk.labelSetMarkup (hlcLabel lc) (T.pack labelText)
      setWorkspaceWidgetStatusClass (workspaceState ws) (hlcLabel lc)
    return lc

hyprlandBuildLabelController :: HyprlandWorkspacesConfig -> HyprlandControllerConstructor
hyprlandBuildLabelController cfg ws = do
  let common = hyprlandWorkspacesCommonConfig cfg
  lbl <- liftIO $ Gtk.labelNew Nothing
  _ <- widgetSetClassGI lbl "workspace-label"
  labelText <- commonLabelSetter common ws
  liftIO $ Gtk.labelSetMarkup lbl (T.pack labelText)
  liftIO $ setWorkspaceWidgetStatusClass (workspaceState ws) lbl
  return $
    HyprlandWWC $
      HyprlandLabelController
        { hlcLabel = lbl,
          hlcLabelSetter = commonLabelSetter common
        }

data HyprlandIconController = HyprlandIconController
  { hicIconsContainer :: Gtk.Box,
    hicIconImages :: [HyprlandIconWidget],
    hicWorkspace :: HyprlandWorkspace,
    hicConfig :: HyprlandWorkspacesConfig
  }

instance HyprlandWorkspaceWidgetController HyprlandIconController where
  hwcGetWidget = Gtk.toWidget . hicIconsContainer
  hwcUpdateWidget ic ws = do
    newImages <-
      if windows ws /= windows (hicWorkspace ic)
        then updateIcons (hicConfig ic) ws (hicIconsContainer ic) (hicIconImages ic)
        else return (hicIconImages ic)
    return ic {hicIconImages = newImages, hicWorkspace = ws}

hyprlandBuildIconController :: HyprlandWorkspacesConfig -> HyprlandControllerConstructor
hyprlandBuildIconController cfg ws = do
  iconsBox <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0
  icons <- updateIcons cfg ws iconsBox []
  return $
    HyprlandWWC $
      HyprlandIconController
        { hicIconsContainer = iconsBox,
          hicIconImages = icons,
          hicWorkspace = ws,
          hicConfig = cfg
        }

data HyprlandContentsController = HyprlandContentsController
  { hccContainerWidget :: Gtk.Widget,
    hccControllers :: [HyprlandWWC]
  }

instance HyprlandWorkspaceWidgetController HyprlandContentsController where
  hwcGetWidget = return . hccContainerWidget
  hwcUpdateWidget cc ws = do
    liftIO $ setWorkspaceWidgetStatusClass (workspaceState ws) (hccContainerWidget cc)
    newControllers <- mapM (`hwcUpdateWidget` ws) (hccControllers cc)
    return cc {hccControllers = newControllers}

hyprlandBuildContentsController ::
  [HyprlandControllerConstructor] -> HyprlandControllerConstructor
hyprlandBuildContentsController constructors ws = do
  controllers <- mapM ($ ws) constructors
  widgets <- liftIO $ mapM hwcGetWidget controllers
  widget <- liftIO $ do
    cons <- Gtk.boxNew Gtk.OrientationHorizontal 0
    mapM_ (Gtk.containerAdd cons) widgets
    _ <- widgetSetClassGI cons "contents"
    Gtk.toWidget cons
  liftIO $ setWorkspaceWidgetStatusClass (workspaceState ws) widget
  return $
    HyprlandWWC $
      HyprlandContentsController
        { hccContainerWidget = widget,
          hccControllers = controllers
        }

hyprlandBuildLabelOverlayController ::
  HyprlandWorkspacesConfig -> HyprlandControllerConstructor
hyprlandBuildLabelOverlayController cfg ws = do
  iconCtrl <- hyprlandBuildIconController cfg ws
  labelCtrl <- hyprlandBuildLabelController cfg ws
  iconWidget <- liftIO $ hwcGetWidget iconCtrl
  labelWidget <- liftIO $ hwcGetWidget labelCtrl
  widget <- buildWorkspaceIconLabelOverlay iconWidget labelWidget
  liftIO $ setWorkspaceWidgetStatusClass (workspaceState ws) widget
  return $
    HyprlandWWC $
      HyprlandContentsController
        { hccContainerWidget = widget,
          hccControllers = [iconCtrl, labelCtrl]
        }

-- | Like 'hyprlandBuildLabelOverlayController' but accepts a custom function
-- to combine the icon and label widgets into a single container widget.
hyprlandBuildCustomOverlayController ::
  (Gtk.Widget -> Gtk.Widget -> TaffyIO Gtk.Widget) ->
  HyprlandWorkspacesConfig ->
  HyprlandControllerConstructor
hyprlandBuildCustomOverlayController combiner cfg ws = do
  iconCtrl <- hyprlandBuildIconController cfg ws
  labelCtrl <- hyprlandBuildLabelController cfg ws
  iconWidget <- liftIO $ hwcGetWidget iconCtrl
  labelWidget <- liftIO $ hwcGetWidget labelCtrl
  widget <- combiner iconWidget labelWidget
  liftIO $ setWorkspaceWidgetStatusClass (workspaceState ws) widget
  return $
    HyprlandWWC $
      HyprlandContentsController
        { hccContainerWidget = widget,
          hccControllers = [iconCtrl, labelCtrl]
        }

hyprlandDefaultBuildContentsController ::
  HyprlandWorkspacesConfig -> HyprlandControllerConstructor
hyprlandDefaultBuildContentsController = hyprlandBuildLabelOverlayController

data HyprlandButtonController = HyprlandButtonController
  { hbcButton :: Gtk.EventBox,
    hbcWorkspaceRef :: IORef HyprlandWorkspace,
    hbcContentsController :: HyprlandWWC
  }

instance HyprlandWorkspaceWidgetController HyprlandButtonController where
  hwcGetWidget = Gtk.toWidget . hbcButton
  hwcUpdateWidget wbc ws = do
    liftIO $ writeIORef (hbcWorkspaceRef wbc) ws
    newContents <- hwcUpdateWidget (hbcContentsController wbc) ws
    return wbc {hbcContentsController = newContents}

hyprlandBuildButtonController ::
  HyprlandWorkspacesConfig -> HyprlandParentControllerConstructor
hyprlandBuildButtonController cfg contentsBuilder ws = do
  cc <- contentsBuilder ws
  ctx <- ask
  contentsWidget <- liftIO $ hwcGetWidget cc
  wsRef <- liftIO $ newIORef ws
  ebox <- liftIO $ do
    eb <- Gtk.eventBoxNew
    Gtk.eventBoxSetVisibleWindow eb False
    Gtk.containerAdd eb contentsWidget
    _ <- Gtk.onWidgetButtonPressEvent eb $ const $ do
      wsCurrent <- readIORef wsRef
      runReaderT (switchToWorkspace cfg wsCurrent) ctx
      return True
    return eb
  return $
    HyprlandWWC $
      HyprlandButtonController
        { hbcButton = ebox,
          hbcWorkspaceRef = wsRef,
          hbcContentsController = cc
        }

defaultHyprlandWidgetBuilder :: HyprlandWorkspacesConfig -> HyprlandControllerConstructor
defaultHyprlandWidgetBuilder cfg =
  hyprlandBuildButtonController cfg (hyprlandDefaultBuildContentsController cfg)

updateIcons ::
  HyprlandWorkspacesConfig ->
  HyprlandWorkspace ->
  Gtk.Box ->
  [HyprlandIconWidget] ->
  ReaderT Context IO [HyprlandIconWidget]
updateIcons cfg ws iconsBox iconWidgets = do
  let common = hyprlandWorkspacesCommonConfig cfg
  sortedWindows <- commonIconSort common $ windows ws
  let (effectiveMinIcons, _targetLen, paddedWindows) =
        computeIconStripLayout (commonMinIcons common) (commonMaxIcons common) sortedWindows
      buildOne i = buildIconWidget (i < effectiveMinIcons) cfg
  syncWidgetPool iconsBox iconWidgets paddedWindows buildOne iconContainer updateIconWidget

buildIconWidget :: Bool -> HyprlandWorkspacesConfig -> ReaderT Context IO HyprlandIconWidget
buildIconWidget transparentOnNone cfg = do
  ctx <- ask
  let common = hyprlandWorkspacesCommonConfig cfg
  strategy <- getScalingImageStrategy
  liftIO $
    mkWorkspaceIconWidget
      strategy
      (Just $ iconSize cfg)
      transparentOnNone
      (\size w -> runReaderT (commonGetWindowIconPixbuf common size w) ctx)
      (`pixBufFromColor` 0)

updateIconWidget :: HyprlandIconWidget -> Maybe HyprlandWindow -> ReaderT Context IO ()
updateIconWidget iconWidget windowData =
  updateWindowIconWidgetState
    iconWidget
    windowData
    (T.pack . windowTitle)
    getWindowStatusString

getWindowStatusString :: HyprlandWindow -> T.Text
getWindowStatusString windowData =
  windowStatusClassFromFlags
    (windowMinimized windowData)
    (windowActive windowData)
    (windowUrgent windowData)

-- | Sort windows by their top-left corner position.
--
-- This mirrors the X11 Workspaces widget default ('sortWindowsByPosition'),
-- but uses Hyprland's @at@ coordinate from @hyprctl clients -j@.
sortHyprlandWindowsByPosition :: [HyprlandWindow] -> [HyprlandWindow]
sortHyprlandWindowsByPosition =
  sortOn $ \w ->
    ( windowMinimized w,
      fromMaybe (999999999, 999999999) (windowAt w)
    )

wLog :: (MonadIO m) => Priority -> String -> m ()
wLog l s = liftIO $ logM "System.Taffybar.Widget.HyprlandWorkspaces" l s

-- Window icon lookup

scaledWindowIconPixbufGetter ::
  HyprlandWindowIconPixbufGetter -> HyprlandWindowIconPixbufGetter
scaledWindowIconPixbufGetter = scaledPixbufGetter

handleIconGetterException ::
  HyprlandWindowIconPixbufGetter -> HyprlandWindowIconPixbufGetter
handleIconGetterException = handlePixbufGetterException wLog

defaultHyprlandGetWindowIconPixbuf :: HyprlandWindowIconPixbufGetter
defaultHyprlandGetWindowIconPixbuf =
  scaledWindowIconPixbufGetter $
    getWindowIconPixbufFromDesktopEntry <|||> getWindowIconPixbufFromClass

getWindowIconPixbufFromClass :: HyprlandWindowIconPixbufGetter
getWindowIconPixbufFromClass size windowData =
  maybeTCombine
    (maybe (return Nothing) (liftIO . getWindowIconFromClasses size) (windowClass windowData))
    (maybe (return Nothing) (liftIO . getWindowIconFromClasses size) (windowInitialClass windowData))

getWindowIconPixbufFromDesktopEntry :: HyprlandWindowIconPixbufGetter
getWindowIconPixbufFromDesktopEntry =
  handleIconGetterException $ \size windowData ->
    maybeTCombine
      (maybe (return Nothing) (getWindowIconFromDesktopEntryByAppId size) (windowClass windowData))
      (maybe (return Nothing) (getWindowIconFromDesktopEntryByAppId size) (windowInitialClass windowData))

getDirectoryEntriesByAppId :: TaffyIO (MM.MultiMap String DesktopEntry)
getDirectoryEntriesByAppId = getStateDefault readDirectoryEntriesByAppId

readDirectoryEntriesByAppId :: TaffyIO (MM.MultiMap String DesktopEntry)
readDirectoryEntriesByAppId =
  liftIO $ indexDesktopEntriesByAppId <$> getDirectoryEntriesDefault

indexDesktopEntriesByAppId :: [DesktopEntry] -> MM.MultiMap String DesktopEntry
indexDesktopEntriesByAppId =
  F.foldl' (\m de -> MM.insert (normalizeAppId $ deFilename de) de m) MM.empty

normalizeAppId :: String -> String
normalizeAppId name =
  let stripped = fromMaybe name (stripSuffix ".desktop" name)
   in map toLower stripped

getWindowIconFromDesktopEntryByAppId ::
  Int32 -> String -> TaffyIO (Maybe Gdk.Pixbuf)
getWindowIconFromDesktopEntryByAppId size appId = do
  entries <- MM.lookup (normalizeAppId appId) <$> getDirectoryEntriesByAppId
  case entries of
    [] -> return Nothing
    (entry : _) -> do
      liftIO $
        logM "System.Taffybar.Widget.HyprlandWorkspaces" DEBUG $
          printf
            "Using desktop entry for icon %s (appId=%s)"
            (deFilename entry)
            appId
      liftIO $ getImageForDesktopEntry size entry

-- Hyprland backend

data HyprlandWorkspaceRef = HyprlandWorkspaceRef
  { hwrId :: Int,
    hwrName :: Text
  }
  deriving (Show, Eq)

instance FromJSON HyprlandWorkspaceRef where
  parseJSON = withObject "HyprlandWorkspaceRef" $ \v ->
    HyprlandWorkspaceRef
      <$> v .: "id"
      <*> v .: "name"

data HyprlandWorkspaceInfo = HyprlandWorkspaceInfo
  { hwiId :: Int,
    hwiName :: Text,
    hwiWindows :: Int
  }
  deriving (Show, Eq)

instance FromJSON HyprlandWorkspaceInfo where
  parseJSON = withObject "HyprlandWorkspaceInfo" $ \v ->
    HyprlandWorkspaceInfo
      <$> v .: "id"
      <*> v .: "name"
      <*> v .:? "windows" .!= 0

data HyprlandMonitorInfo = HyprlandMonitorInfo
  { hmFocused :: Bool,
    hmActiveWorkspace :: Maybe HyprlandWorkspaceRef
  }
  deriving (Show, Eq)

instance FromJSON HyprlandMonitorInfo where
  parseJSON = withObject "HyprlandMonitorInfo" $ \v ->
    HyprlandMonitorInfo
      <$> v .:? "focused" .!= False
      <*> v .:? "activeWorkspace"

data HyprlandClient = HyprlandClient
  { hcAddress :: Text,
    hcTitle :: Text,
    hcInitialTitle :: Maybe Text,
    hcClass :: Maybe Text,
    hcInitialClass :: Maybe Text,
    hcAt :: Maybe (Int, Int),
    hcWorkspace :: HyprlandWorkspaceRef,
    hcFocused :: Bool,
    hcHidden :: Bool,
    hcMapped :: Bool,
    hcUrgent :: Bool
  }
  deriving (Show, Eq)

instance FromJSON HyprlandClient where
  parseJSON = withObject "HyprlandClient" $ \v ->
    HyprlandClient
      <$> v .: "address"
      <*> v .:? "title" .!= ""
      <*> v .:? "initialTitle"
      <*> v .:? "class"
      <*> v .:? "initialClass"
      <*> (vec2FromList <$> v .:? "at")
      <*> v .: "workspace"
      <*> v .:? "focused" .!= False
      <*> v .:? "hidden" .!= False
      <*> v .:? "mapped" .!= True
      <*> v .:? "urgent" .!= False
    where
      vec2FromList :: Maybe [Int] -> Maybe (Int, Int)
      vec2FromList (Just [x, y]) = Just (x, y)
      vec2FromList _ = Nothing

newtype HyprlandActiveWindow = HyprlandActiveWindow
  { hawAddress :: Text
  }
  deriving (Show, Eq)

instance FromJSON HyprlandActiveWindow where
  parseJSON = withObject "HyprlandActiveWindow" $ \v ->
    HyprlandActiveWindow <$> v .: "address"

runHyprctlJson :: (FromJSON a) => [String] -> TaffyIO (Either String a)
runHyprctlJson args = do
  let args' =
        case args of
          ("-j" : rest) -> rest
          _ -> args
  result <- runHyprlandCommandJsonT (Hypr.hyprCommandJson args')
  pure $ case result of
    Left err -> Left (show err)
    Right out -> Right out

runHyprlandCommandRaw :: [String] -> TaffyIO (Either String BS.ByteString)
runHyprlandCommandRaw args = do
  let cmd =
        case args of
          ("-j" : rest) -> Hypr.hyprCommandJson rest
          _ -> Hypr.hyprCommand args
  result <- runHyprlandCommandRawT cmd
  pure $ case result of
    Left err -> Left (show err)
    Right out -> Right out

hyprlandSwitchToWorkspace :: HyprlandWorkspace -> TaffyIO ()
hyprlandSwitchToWorkspace ws = do
  result <- runHyprlandCommandRaw ["dispatch", "workspace", workspaceName ws]
  case result of
    Left err -> wLog WARNING $ printf "Failed to switch workspace: %s" err
    Right _ -> return ()

getHyprlandWorkspaces :: TaffyIO [HyprlandWorkspace]
getHyprlandWorkspaces = do
  workspacesResult <- runHyprctlJson ["-j", "workspaces"]
  clientsResult <- runHyprctlJson ["-j", "clients"]
  monitorsResult <- runHyprctlJson ["-j", "monitors"]
  activeWorkspaceResult <- runHyprctlJson ["-j", "activeworkspace"]
  activeWindowAddress <- getActiveWindowAddress

  workspaces <- case workspacesResult of
    Left err -> wLog WARNING (printf "hyprctl workspaces failed: %s" err) >> return []
    Right ws -> return ws

  let workspacesCount = length workspaces
      workspacesWindowsSum = sum (map hwiWindows workspaces)
  (clientsOk, clients) <- case clientsResult of
    Left err -> do
      wLog WARNING $
        printf
          "hyprctl clients failed: %s (workspaces=%d windowsSum=%d)"
          err
          workspacesCount
          workspacesWindowsSum
      return (False, [])
    Right cs -> return (True, cs)

  monitors <- case monitorsResult of
    Left err -> wLog WARNING (printf "hyprctl monitors failed: %s" err) >> return []
    Right ms -> return ms

  activeWorkspace <- case activeWorkspaceResult of
    Left err -> wLog WARNING (printf "hyprctl activeworkspace failed: %s" err) >> return Nothing
    Right ws -> return $ Just ws

  buildWorkspacesFromHyprland
    workspaces
    clientsOk
    clients
    monitors
    activeWorkspace
    activeWindowAddress

getActiveWindowAddress :: TaffyIO (Maybe Text)
getActiveWindowAddress = do
  result <- runHyprctlJson ["-j", "activewindow"]
  case result of
    Left _ -> return Nothing
    Right activeWindow -> return $ Just $ hawAddress activeWindow

buildWorkspacesFromHyprland ::
  [HyprlandWorkspaceInfo] ->
  Bool ->
  [HyprlandClient] ->
  [HyprlandMonitorInfo] ->
  Maybe HyprlandWorkspaceRef ->
  Maybe Text ->
  TaffyIO [HyprlandWorkspace]
buildWorkspacesFromHyprland workspaces clientsOk clients monitors activeWorkspace activeWindowAddress = do
  let windowsByWorkspace = collectWorkspaceWindows activeWindowAddress clients
      sortedWorkspaces = sortOn hwiId workspaces
      visibleWorkspaceIds =
        map hwrId $ mapMaybe hmActiveWorkspace monitors
      focusedWorkspaceId =
        listToMaybe
          [ hwrId ws
          | m <- monitors,
            hmFocused m,
            Just ws <- [hmActiveWorkspace m]
          ]
      activeWorkspaceId =
        focusedWorkspaceId <|> fmap hwrId activeWorkspace

  return $ map (buildWorkspace windowsByWorkspace visibleWorkspaceIds activeWorkspaceId) sortedWorkspaces
  where
    buildWorkspace windowsMap visibleIds activeId wsInfo =
      let wsId = hwiId wsInfo
          wsName = hwiName wsInfo
          wins = M.findWithDefault [] wsId windowsMap
          hasWindows = not (null wins) || hwiWindows wsInfo > 0
          state
            | Just wsId == activeId = Active
            | wsId `elem` visibleIds = Visible
            -- If we couldn't fetch clients, don't aggressively hide workspaces.
            -- This prevents "invisible" workspaces on transient Hyprland restarts.
            | not hasWindows && clientsOk = Empty
            | otherwise = Hidden
       in HyprlandWorkspace
            { workspaceIdx = wsId,
              workspaceName = T.unpack wsName,
              workspaceState = state,
              windows = wins
            }

collectWorkspaceWindows :: Maybe Text -> [HyprlandClient] -> M.Map Int [HyprlandWindow]
collectWorkspaceWindows activeWindowAddress =
  F.foldl' (addWindow activeWindowAddress) M.empty
  where
    addWindow activeAddr windowsMap client =
      let wsId = hwrId (hcWorkspace client)
          windowData = windowFromClient activeAddr client
       in M.insertWith (++) wsId [windowData] windowsMap

windowFromClient :: Maybe Text -> HyprlandClient -> HyprlandWindow
windowFromClient activeAddr client =
  let titleText =
        if T.null (hcTitle client)
          then fromMaybe "" (hcInitialTitle client)
          else hcTitle client
      active =
        hcFocused client || Just (hcAddress client) == activeAddr
      minimized =
        hcHidden client || not (hcMapped client)
   in HyprlandWindow
        { windowAddress = hcAddress client,
          windowTitle = T.unpack titleText,
          windowClass = T.unpack <$> hcClass client,
          windowInitialClass = T.unpack <$> hcInitialClass client,
          windowAt = hcAt client,
          windowUrgent = hcUrgent client,
          windowActive = active,
          windowMinimized = minimized
        }
