{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.HyprlandWorkspaces
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Hyprland workspaces widget backed by hyprctl.
-----------------------------------------------------------------------------

module System.Taffybar.Widget.HyprlandWorkspaces where

import           Control.Applicative ((<|>))
import           Control.Concurrent (forkIO, killThread)
import           Control.Concurrent.STM.TChan (TChan, readTChan)
import           Control.Monad (foldM, forM_, when)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.STM (atomically)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson (FromJSON(..), withObject, (.:), (.:?), (.!=))
import           Data.Char (toLower)
import qualified Data.ByteString as BS
import           Data.Default (Default(..))
import qualified Data.Foldable as F
import           Data.Int (Int32)
import           Data.List (intercalate, sortOn, stripPrefix)
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Log.Logger (Priority(..), logM)
import           Text.Printf (printf)

import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk

import           System.Environment.XDG.DesktopEntry
  ( DesktopEntry
  , deFilename
  , getDirectoryEntriesDefault
  )
import           System.Taffybar.Context
import           System.Taffybar.Hyprland
  ( getHyprlandEventChan
  , runHyprlandCommandJsonT
  , runHyprlandCommandRawT
  )
import qualified System.Taffybar.Information.Hyprland as Hypr
import           System.Taffybar.Util
import           System.Taffybar.Widget.Util
  ( WindowIconWidget(..)
  , computeIconStripLayout
  , getImageForDesktopEntry
  , handlePixbufGetterException
  , scaledPixbufGetter
  , syncWidgetPool
  , updateWidgetClasses
  , updateWindowIconWidgetState
  , widgetSetClassGI
  , windowStatusClassFromFlags
  )
import           System.Taffybar.Widget.Workspaces.Shared
  ( buildWorkspaceIconLabelOverlay
  , mkWorkspaceIconWidget
  )
import           System.Taffybar.Widget.Workspaces
  ( WorkspaceState(..)
  , cssWorkspaceStates
  , getCSSClass
  )
import           System.Taffybar.WindowIcon (getWindowIconFromClasses, pixBufFromColor)

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix value =
  reverse <$> stripPrefix (reverse suffix) (reverse value)

isSpecialHyprWorkspace :: HyprlandWorkspace -> Bool
isSpecialHyprWorkspace ws =
  let name = T.toLower $ T.pack $ workspaceName ws
  in T.isPrefixOf "special" name || workspaceIdx ws < 0

data HyprlandWindow = HyprlandWindow
  { windowAddress :: Text
  , windowTitle :: String
  , windowClass :: Maybe String
  , windowInitialClass :: Maybe String
  , windowUrgent :: Bool
  , windowActive :: Bool
  , windowMinimized :: Bool
  } deriving (Show, Eq)

data HyprlandWorkspace = HyprlandWorkspace
  { workspaceIdx :: Int
  , workspaceName :: String
  , workspaceState :: WorkspaceState
  , windows :: [HyprlandWindow]
  } deriving (Show, Eq)

newtype HyprlandWorkspaceCache = HyprlandWorkspaceCache [HyprlandWorkspace]

newtype HyprlandWorkspaceWidgetCache
  = HyprlandWorkspaceWidgetCache (M.Map Int HyprlandWorkspaceWidgetState)

newtype HyprlandWorkspaceOrderCache
  = HyprlandWorkspaceOrderCache [Int]

data HyprlandWorkspaceWidgetState = HyprlandWorkspaceWidgetState
  { workspaceWrapper :: Gtk.Widget
  , workspaceButton :: Gtk.EventBox
  , workspaceRef :: IORef HyprlandWorkspace
  , workspaceContents :: Gtk.Widget
  , workspaceLabel :: Gtk.Label
  , workspaceIconsBox :: Gtk.Box
  , workspaceIcons :: [HyprlandIconWidget]
  , workspaceLast :: HyprlandWorkspace
  }

type HyprlandIconWidget = WindowIconWidget HyprlandWindow

type HyprlandWindowIconPixbufGetter =
  Int32 -> HyprlandWindow -> TaffyIO (Maybe Gdk.Pixbuf)

data HyprlandWorkspacesConfig =
  HyprlandWorkspacesConfig
  { getWorkspaces :: TaffyIO [HyprlandWorkspace]
  , switchToWorkspace :: HyprlandWorkspace -> TaffyIO ()
  , updateIntervalSeconds :: Double
  -- | Build the per-workspace widget from the icon strip widget and the label
  -- widget. This allows X11 and Hyprland workspaces to share the same CSS and
  -- layout logic.
  , widgetBuilder :: Gtk.Widget -> Gtk.Widget -> TaffyIO Gtk.Widget
  , widgetGap :: Int
  , maxIcons :: Maybe Int
  , minIcons :: Int
  , iconSize :: Int32
  , getWindowIconPixbuf :: HyprlandWindowIconPixbufGetter
  , labelSetter :: HyprlandWorkspace -> TaffyIO String
  , showWorkspaceFn :: HyprlandWorkspace -> Bool
  , iconSort :: [HyprlandWindow] -> TaffyIO [HyprlandWindow]
  , urgentWorkspaceState :: Bool
  }

defaultHyprlandWorkspacesConfig :: HyprlandWorkspacesConfig
defaultHyprlandWorkspacesConfig =
  HyprlandWorkspacesConfig
  { getWorkspaces = getHyprlandWorkspaces
  , switchToWorkspace = hyprlandSwitchToWorkspace
  , updateIntervalSeconds = 1
  , widgetBuilder = buildWorkspaceIconLabelOverlay
  , widgetGap = 0
  , maxIcons = Nothing
  , minIcons = 0
  , iconSize = 16
  , getWindowIconPixbuf = defaultHyprlandGetWindowIconPixbuf
  , labelSetter = return . workspaceName
  , showWorkspaceFn = \ws ->
      workspaceState ws /= Empty && not (isSpecialHyprWorkspace ws)
  , iconSort = return
  , urgentWorkspaceState = False
  }

instance Default HyprlandWorkspacesConfig where
  def = defaultHyprlandWorkspacesConfig

hyprlandWorkspacesNew :: HyprlandWorkspacesConfig -> TaffyIO Gtk.Widget
hyprlandWorkspacesNew cfg = do
  cont <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal $
          fromIntegral (widgetGap cfg)
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
  in eventName `elem`
     [ "workspace"
     , "workspacev2"
     , "focusedmon"
     , "activewindow"
     , "activewindowv2"
     , "openwindow"
     , "closewindow"
     , "movewindow"
     , "movewindowv2"
     , "moveworkspace"
     , "renameworkspace"
     , "createworkspace"
     , "destroyworkspace"
     , "monitoradded"
     , "monitorremoved"
     -- Synthetic "event" emitted by our event reader thread whenever it
     -- (re)connects to Hyprland. This lets us refresh after a compositor
     -- restart without polling.
     , "taffybar-hyprland-connected"
     ]

refreshWorkspaces :: HyprlandWorkspacesConfig -> Gtk.Box -> ReaderT Context IO ()
refreshWorkspaces cfg cont = do
  ws <- getWorkspaces cfg
  HyprlandWorkspaceCache prev <- getStateDefault $ return (HyprlandWorkspaceCache [])
  let ignoreEmptyResult = null ws && not (null prev)
  when ignoreEmptyResult $
    liftIO $ wLog WARNING $
      printf
        "Hyprland workspaces refresh returned empty list; retaining previous state (prevTotal=%d)."
        (length prev)
  when (ws /= prev && not ignoreEmptyResult) $ do
    liftIO $ wLog DEBUG $
      printf "Hyprland workspaces refresh: total=%d shown=%d (minIcons=%d maxIcons=%s) %s"
        (length ws)
        (length (filter (showWorkspaceFn cfg) ws))
        (minIcons cfg)
        (show (maxIcons cfg))
        (summarizeWorkspaces ws)
    _ <- setState (HyprlandWorkspaceCache ws)
    ctx <- ask
    liftIO $ postGUIASync $ runReaderT (renderWorkspaces cfg cont ws) ctx

renderWorkspaces ::
  HyprlandWorkspacesConfig -> Gtk.Box -> [HyprlandWorkspace] -> ReaderT Context IO ()
renderWorkspaces cfg cont workspaces = do
  let workspaces' = map (applyUrgentState cfg) workspaces
  HyprlandWorkspaceWidgetCache widgetCache <-
    getStateDefault $ return (HyprlandWorkspaceWidgetCache M.empty)
  HyprlandWorkspaceOrderCache prevOrder <-
    getStateDefault $ return (HyprlandWorkspaceOrderCache [])
  let oldCache = widgetCache
      buildOrUpdate newCache ws = do
        let idx = workspaceIdx ws
        state <- case M.lookup idx oldCache of
          Just prevState
            | workspaceLast prevState == ws -> return prevState
            | otherwise -> updateWorkspaceWidgetState cfg ws prevState
          Nothing -> buildWorkspaceWidgetState cfg ws
        return (M.insert idx state newCache, state)

  (newCache, orderedStatesRev) <-
    foldM
      (\(cacheAcc, statesAcc) ws -> do
          (cacheAcc', st) <- buildOrUpdate cacheAcc ws
          return (cacheAcc', (ws, st) : statesAcc)
      )
      (M.empty, [])
      workspaces'
  let orderedStates = reverse orderedStatesRev

  let primaryShowFn = showWorkspaceFn cfg
      primaryShownCount = length $ filter (primaryShowFn . fst) orderedStates
      fallbackShowFn ws = workspaceState ws /= Empty
      fallbackShownCount = length $ filter (fallbackShowFn . fst) orderedStates
      (finalShowFn, finalShownCount, fallbackUsed) =
        if primaryShownCount == 0 && fallbackShownCount > 0
          then (fallbackShowFn, fallbackShownCount, True)
          else (primaryShowFn, primaryShownCount, False)

  when (fallbackUsed && not (null orderedStates)) $
    liftIO $ wLog WARNING $
      printf
        "Hyprland workspaces: showWorkspaceFn hid all %d workspaces; falling back to showing non-empty workspaces (including special:*). %s"
        (length orderedStates)
        (summarizeWorkspaces (map fst orderedStates))
  when (finalShownCount == 0 && not (null orderedStates)) $
    liftIO $ wLog WARNING $
      printf
        "Hyprland workspaces widget is blank: no workspaces passed the show filter (total=%d). %s"
        (length orderedStates)
        (summarizeWorkspaces (map fst orderedStates))

  -- Remove wrappers for workspaces that disappeared.
  let removed = M.difference oldCache newCache
  forM_ (M.elems removed) $ \st ->
    liftIO $ Gtk.containerRemove cont (workspaceWrapper st)

  -- Add wrappers for newly created workspaces.
  let added = M.difference newCache oldCache
  forM_ (M.elems added) $ \st -> do
    liftIO $ Gtk.containerAdd cont (workspaceWrapper st)
    liftIO $ Gtk.widgetShowAll (workspaceWrapper st)

  let desiredOrder = map (workspaceIdx . fst) orderedStates
      needsReorder =
        desiredOrder /= prevOrder || not (M.null added) || not (M.null removed)
  when needsReorder $ do
    -- Reorder wrappers to match the order returned by hyprctl.
    forM_ (zip [0 :: Int ..] orderedStates) $ \(pos, (_ws, st)) ->
      liftIO $ Gtk.boxReorderChild cont (workspaceWrapper st) (fromIntegral pos)

  -- Show/hide the clickable workspace widget without removing it from the box.
  forM_ orderedStates $ \(ws, st) ->
    if finalShowFn ws
      then liftIO $ Gtk.widgetShow (workspaceButton st)
      else liftIO $ Gtk.widgetHide (workspaceButton st)

  _ <- setState (HyprlandWorkspaceWidgetCache newCache)
  _ <- setState (HyprlandWorkspaceOrderCache desiredOrder)
  return ()

summarizeWorkspaces :: [HyprlandWorkspace] -> String
summarizeWorkspaces wss =
  let summarizeOne ws =
        printf "%d:%s:%s(wins=%d)"
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
  | urgentWorkspaceState cfg
    && workspaceState ws == Hidden
    && any windowUrgent (windows ws) =
      ws { workspaceState = Urgent }
  | otherwise = ws

buildWorkspaceWidgetState ::
  HyprlandWorkspacesConfig -> HyprlandWorkspace -> ReaderT Context IO HyprlandWorkspaceWidgetState
buildWorkspaceWidgetState cfg ws = do
  ctx <- ask
  labelText <- labelSetter cfg ws
  label <- liftIO $ Gtk.labelNew Nothing
  _ <- widgetSetClassGI label "workspace-label"
  liftIO $ Gtk.labelSetMarkup label (T.pack labelText)

  iconsBox <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0
  wsRef <- liftIO $ newIORef ws
  icons <- updateIcons cfg ws iconsBox []

  -- Match the X11 Workspaces widget's overlay layout:
  -- icons are the main contents and the label is overlaid bottom-left.
  iconsWidget <- liftIO $ Gtk.toWidget iconsBox
  labelWidget <- liftIO $ Gtk.toWidget label
  overlayWidget <- widgetBuilder cfg iconsWidget labelWidget
  setWorkspaceWidgetStatusClass ws overlayWidget
  -- X11 Workspaces applies workspace state classes to the label too.
  setWorkspaceWidgetStatusClass ws label

  ebox <- liftIO Gtk.eventBoxNew
  Gtk.eventBoxSetVisibleWindow ebox False
  liftIO $ Gtk.containerAdd ebox overlayWidget
  _ <- liftIO $
       Gtk.onWidgetButtonPressEvent ebox $ const $ do
         wsCurrent <- readIORef wsRef
         runReaderT (switchToWorkspace cfg wsCurrent) ctx
         return True
  wrapperBox <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0
  liftIO $ Gtk.containerAdd wrapperBox =<< Gtk.toWidget ebox
  wrapperWidget <- Gtk.toWidget wrapperBox
  return HyprlandWorkspaceWidgetState
    { workspaceWrapper = wrapperWidget
    , workspaceButton = ebox
    , workspaceRef = wsRef
    , workspaceContents = overlayWidget
    , workspaceLabel = label
    , workspaceIconsBox = iconsBox
    , workspaceIcons = icons
    , workspaceLast = ws
    }

updateWorkspaceWidgetState ::
  HyprlandWorkspacesConfig
  -> HyprlandWorkspace
  -> HyprlandWorkspaceWidgetState
  -> ReaderT Context IO HyprlandWorkspaceWidgetState
updateWorkspaceWidgetState cfg ws state = do
  labelText <- labelSetter cfg ws
  liftIO $ Gtk.labelSetMarkup (workspaceLabel state) (T.pack labelText)
  liftIO $ writeIORef (workspaceRef state) ws
  icons <-
    if windows ws /= windows (workspaceLast state)
      then updateIcons cfg ws (workspaceIconsBox state) (workspaceIcons state)
      else return (workspaceIcons state)
  setWorkspaceWidgetStatusClass ws (workspaceContents state)
  setWorkspaceWidgetStatusClass ws (workspaceLabel state)
  return state { workspaceLast = ws, workspaceIcons = icons }

updateIcons ::
  HyprlandWorkspacesConfig
  -> HyprlandWorkspace
  -> Gtk.Box
  -> [HyprlandIconWidget]
  -> ReaderT Context IO [HyprlandIconWidget]
updateIcons cfg ws iconsBox iconWidgets = do
  sortedWindows <- iconSort cfg $ windows ws
  let (effectiveMinIcons, _targetLen, paddedWindows) =
        computeIconStripLayout (minIcons cfg) (maxIcons cfg) sortedWindows
      buildOne i = buildIconWidget (i < effectiveMinIcons) cfg
  syncWidgetPool iconsBox iconWidgets paddedWindows buildOne iconContainer updateIconWidget

buildIconWidget :: Bool -> HyprlandWorkspacesConfig -> ReaderT Context IO HyprlandIconWidget
buildIconWidget transparentOnNone cfg = do
  ctx <- ask
  liftIO $
    mkWorkspaceIconWidget
      (Just $ iconSize cfg)
      transparentOnNone
      (\size w -> runReaderT (getWindowIconPixbuf cfg size w) ctx)
      (\size -> pixBufFromColor size 0)

updateIconWidget :: HyprlandIconWidget -> Maybe HyprlandWindow -> ReaderT Context IO ()
updateIconWidget iconWidget windowData =
  updateWindowIconWidgetState
    iconWidget
    windowData
    (T.pack . windowTitle)
    getWindowStatusString

setWorkspaceWidgetStatusClass ::
  (MonadIO m, Gtk.IsWidget a) => HyprlandWorkspace -> a -> m ()
setWorkspaceWidgetStatusClass workspace widget =
  updateWidgetClasses
    widget
    [getCSSClass $ workspaceState workspace]
    cssWorkspaceStates

getWindowStatusString :: HyprlandWindow -> T.Text
getWindowStatusString windowData =
  windowStatusClassFromFlags
    (windowMinimized windowData)
    (windowActive windowData)
    (windowUrgent windowData)

wLog :: MonadIO m => Priority -> String -> m ()
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
    (entry:_) -> do
      liftIO $ logM "System.Taffybar.Widget.HyprlandWorkspaces" DEBUG $
        printf "Using desktop entry for icon %s (appId=%s)"
               (deFilename entry) appId
      liftIO $ getImageForDesktopEntry size entry

-- Hyprland backend

data HyprlandWorkspaceRef = HyprlandWorkspaceRef
  { hwrId :: Int
  , hwrName :: Text
  } deriving (Show, Eq)

instance FromJSON HyprlandWorkspaceRef where
  parseJSON = withObject "HyprlandWorkspaceRef" $ \v ->
    HyprlandWorkspaceRef
      <$> v .: "id"
      <*> v .: "name"

data HyprlandWorkspaceInfo = HyprlandWorkspaceInfo
  { hwiId :: Int
  , hwiName :: Text
  , hwiWindows :: Int
  } deriving (Show, Eq)

instance FromJSON HyprlandWorkspaceInfo where
  parseJSON = withObject "HyprlandWorkspaceInfo" $ \v ->
    HyprlandWorkspaceInfo
      <$> v .: "id"
      <*> v .: "name"
      <*> v .:? "windows" .!= 0

data HyprlandMonitorInfo = HyprlandMonitorInfo
  { hmFocused :: Bool
  , hmActiveWorkspace :: Maybe HyprlandWorkspaceRef
  } deriving (Show, Eq)

instance FromJSON HyprlandMonitorInfo where
  parseJSON = withObject "HyprlandMonitorInfo" $ \v ->
    HyprlandMonitorInfo
      <$> v .:? "focused" .!= False
      <*> v .:? "activeWorkspace"

data HyprlandClient = HyprlandClient
  { hcAddress :: Text
  , hcTitle :: Text
  , hcInitialTitle :: Maybe Text
  , hcClass :: Maybe Text
  , hcInitialClass :: Maybe Text
  , hcWorkspace :: HyprlandWorkspaceRef
  , hcFocused :: Bool
  , hcHidden :: Bool
  , hcMapped :: Bool
  , hcUrgent :: Bool
  } deriving (Show, Eq)

instance FromJSON HyprlandClient where
  parseJSON = withObject "HyprlandClient" $ \v ->
    HyprlandClient
      <$> v .: "address"
      <*> v .:? "title" .!= ""
      <*> v .:? "initialTitle"
      <*> v .:? "class"
      <*> v .:? "initialClass"
      <*> v .: "workspace"
      <*> v .:? "focused" .!= False
      <*> v .:? "hidden" .!= False
      <*> v .:? "mapped" .!= True
      <*> v .:? "urgent" .!= False

newtype HyprlandActiveWindow = HyprlandActiveWindow
  { hawAddress :: Text
  } deriving (Show, Eq)

instance FromJSON HyprlandActiveWindow where
  parseJSON = withObject "HyprlandActiveWindow" $ \v ->
    HyprlandActiveWindow <$> v .: "address"

runHyprctlJson :: FromJSON a => [String] -> TaffyIO (Either String a)
runHyprctlJson args = do
  let args' =
        case args of
          ("-j":rest) -> rest
          _ -> args
  result <- runHyprlandCommandJsonT (Hypr.hyprCommandJson args')
  pure $ case result of
    Left err -> Left (show err)
    Right out -> Right out

runHyprlandCommandRaw :: [String] -> TaffyIO (Either String BS.ByteString)
runHyprlandCommandRaw args = do
  let cmd =
        case args of
          ("-j":rest) -> Hypr.hyprCommandJson rest
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
  [HyprlandWorkspaceInfo]
  -> Bool
  -> [HyprlandClient]
  -> [HyprlandMonitorInfo]
  -> Maybe HyprlandWorkspaceRef
  -> Maybe Text
  -> TaffyIO [HyprlandWorkspace]
buildWorkspacesFromHyprland workspaces clientsOk clients monitors activeWorkspace activeWindowAddress = do
  let windowsByWorkspace = collectWorkspaceWindows activeWindowAddress clients
      sortedWorkspaces = sortOn hwiId workspaces
      visibleWorkspaceIds =
        map hwrId $ mapMaybe hmActiveWorkspace monitors
      focusedWorkspaceId =
        listToMaybe [ hwrId ws
                    | m <- monitors
                    , hmFocused m
                    , Just ws <- [hmActiveWorkspace m]
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
           { workspaceIdx = wsId
           , workspaceName = T.unpack wsName
           , workspaceState = state
           , windows = wins
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
       { windowAddress = hcAddress client
       , windowTitle = T.unpack titleText
       , windowClass = T.unpack <$> hcClass client
       , windowInitialClass = T.unpack <$> hcInitialClass client
       , windowUrgent = hcUrgent client
       , windowActive = active
       , windowMinimized = minimized
       }
