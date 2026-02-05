{-# LANGUAGE DeriveGeneric #-}
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
import           Control.Concurrent (killThread)
import           Control.Exception.Enclosed (catchAny)
import           Control.Monad (forM_, unless, when, (>=>))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson (FromJSON(..), eitherDecode', withObject, (.:), (.:?), (.!=))
import           Data.Char (toLower)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default (Default(..))
import           Data.Int (Int32)
import           Data.List (foldl', sortOn, stripPrefix)
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Log.Logger (Priority(..), logM)
import           Text.Printf (printf)

import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import           StatusNotifier.Tray (scalePixbufToSize)

import           System.Environment.XDG.DesktopEntry
  ( DesktopEntry
  , deFilename
  , getDirectoryEntriesDefault
  )
import           System.Taffybar.Context
import           System.Taffybar.Util
import           System.Taffybar.Widget.Util
  ( buildContentsBox
  , getImageForDesktopEntry
  , widgetSetClassGI
  )
import           System.Taffybar.WindowIcon (getWindowIconFromClasses, pixBufFromColor)

data WorkspaceState
  = Active
  | Visible
  | Hidden
  | Empty
  | Urgent
  deriving (Show, Eq)

getCSSClass :: (Show s) => s -> T.Text
getCSSClass = T.toLower . T.pack . show

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix value =
  reverse <$> stripPrefix (reverse suffix) (reverse value)

cssWorkspaceStates :: [T.Text]
cssWorkspaceStates = map getCSSClass [Active, Visible, Hidden, Empty, Urgent]

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

type HyprlandWindowIconPixbufGetter =
  Int32 -> HyprlandWindow -> TaffyIO (Maybe Gdk.Pixbuf)

data HyprlandWorkspacesConfig =
  HyprlandWorkspacesConfig
  { getWorkspaces :: TaffyIO [HyprlandWorkspace]
  , switchToWorkspace :: HyprlandWorkspace -> TaffyIO ()
  , updateIntervalSeconds :: Double
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
  , widgetGap = 0
  , maxIcons = Nothing
  , minIcons = 0
  , iconSize = 16
  , getWindowIconPixbuf = defaultHyprlandGetWindowIconPixbuf
  , labelSetter = return . workspaceName
  , showWorkspaceFn = const True
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
  _ <- liftIO refresh
  tid <- liftIO $ foreverWithDelay (updateIntervalSeconds cfg) refresh
  _ <- liftIO $ Gtk.onWidgetUnrealize cont $ killThread tid
  Gtk.toWidget cont

refreshWorkspaces :: HyprlandWorkspacesConfig -> Gtk.Box -> ReaderT Context IO ()
refreshWorkspaces cfg cont = do
  ws <- getWorkspaces cfg
  ctx <- ask
  liftIO $ postGUIASync $ runReaderT (renderWorkspaces cfg cont ws) ctx

renderWorkspaces ::
  HyprlandWorkspacesConfig -> Gtk.Box -> [HyprlandWorkspace] -> ReaderT Context IO ()
renderWorkspaces cfg cont workspaces = do
  liftIO $ Gtk.containerForeach cont (Gtk.containerRemove cont)
  let visibleWorkspaces =
        map (applyUrgentState cfg) $ filter (showWorkspaceFn cfg) workspaces
  forM_ visibleWorkspaces $ \ws -> do
    widget <- buildWorkspaceWidget cfg ws
    liftIO $ Gtk.containerAdd cont widget
  liftIO $ Gtk.widgetShowAll cont

applyUrgentState :: HyprlandWorkspacesConfig -> HyprlandWorkspace -> HyprlandWorkspace
applyUrgentState cfg ws
  | urgentWorkspaceState cfg
    && workspaceState ws == Hidden
    && any windowUrgent (windows ws) =
      ws { workspaceState = Urgent }
  | otherwise = ws

buildWorkspaceWidget ::
  HyprlandWorkspacesConfig -> HyprlandWorkspace -> ReaderT Context IO Gtk.Widget
buildWorkspaceWidget cfg ws = do
  ctx <- ask
  labelText <- labelSetter cfg ws
  label <- liftIO $ Gtk.labelNew (Just $ T.pack labelText)
  _ <- widgetSetClassGI label "workspace-label"

  iconsBox <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0
  sortedWindows <- iconSort cfg $ windows ws
  let windowCount = length sortedWindows
      maxNeeded = maybe windowCount (min windowCount) (maxIcons cfg)
      shownWindows = map Just $ take maxNeeded sortedWindows
      paddedWindows =
        shownWindows ++ replicate (max 0 (minIcons cfg - length shownWindows)) Nothing
  forM_ paddedWindows $ \windowData -> do
    iconWidget <- buildIconWidget cfg windowData
    liftIO $ Gtk.containerAdd iconsBox iconWidget

  inner <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0
  liftIO $ Gtk.containerAdd inner label
  liftIO $ Gtk.containerAdd inner iconsBox
  contents <- buildContentsBox =<< Gtk.toWidget inner
  setWorkspaceWidgetStatusClass ws contents

  ebox <- liftIO Gtk.eventBoxNew
  Gtk.eventBoxSetVisibleWindow ebox False
  liftIO $ Gtk.containerAdd ebox contents
  _ <- liftIO $
       Gtk.onWidgetButtonPressEvent ebox $ const $ do
         runReaderT (switchToWorkspace cfg ws) ctx
         return True
  Gtk.toWidget ebox

buildIconWidget ::
  HyprlandWorkspacesConfig -> Maybe HyprlandWindow -> ReaderT Context IO Gtk.Widget
buildIconWidget cfg windowData = do
  iconButton <- liftIO Gtk.eventBoxNew
  icon <- liftIO Gtk.imageNew
  _ <- widgetSetClassGI icon "window-icon"
  _ <- widgetSetClassGI iconButton "window-icon-container"
  liftIO $ Gtk.containerAdd iconButton icon

  pixbuf <- case windowData of
    Nothing -> Just <$> pixBufFromColor (iconSize cfg) 0
    Just w -> getWindowIconPixbuf cfg (iconSize cfg) w
  liftIO $ Gtk.imageSetFromPixbuf icon pixbuf

  case windowData of
    Nothing -> return ()
    Just w -> liftIO $ Gtk.widgetSetTooltipText iconButton $
              Just $ T.pack $ windowTitle w

  let statusString =
        maybe "inactive" getWindowStatusString windowData
  updateWidgetClasses iconButton [statusString] possibleStatusStrings

  Gtk.toWidget iconButton

setWorkspaceWidgetStatusClass ::
  (MonadIO m, Gtk.IsWidget a) => HyprlandWorkspace -> a -> m ()
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

getWindowStatusString :: HyprlandWindow -> T.Text
getWindowStatusString windowData = T.toLower $ T.pack $
  case windowData of
    HyprlandWindow { windowMinimized = True } -> "minimized"
    HyprlandWindow { windowActive = True } -> show Active
    HyprlandWindow { windowUrgent = True } -> show Urgent
    _ -> "normal"

possibleStatusStrings :: [T.Text]
possibleStatusStrings =
  map
    (T.toLower . T.pack)
    [show Active, show Urgent, "minimized", "normal", "inactive"]

wLog :: MonadIO m => Priority -> String -> m ()
wLog l s = liftIO $ logM "System.Taffybar.Widget.HyprlandWorkspaces" l s

-- Window icon lookup

scaledWindowIconPixbufGetter ::
  HyprlandWindowIconPixbufGetter -> HyprlandWindowIconPixbufGetter
scaledWindowIconPixbufGetter getter size =
  getter size >=>
  liftIO . traverse (scalePixbufToSize size Gtk.OrientationHorizontal)

handleIconGetterException ::
  HyprlandWindowIconPixbufGetter -> HyprlandWindowIconPixbufGetter
handleIconGetterException getter size windowData =
  catchAny (getter size windowData) $ \e -> do
    wLog WARNING $ printf "Failed to get window icon for %s: %s"
                        (show windowData) (show e)
    return Nothing

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
  foldl' (\m de -> MM.insert (normalizeAppId $ deFilename de) de m) MM.empty

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
  } deriving (Show, Eq)

instance FromJSON HyprlandWorkspaceInfo where
  parseJSON = withObject "HyprlandWorkspaceInfo" $ \v ->
    HyprlandWorkspaceInfo
      <$> v .: "id"
      <*> v .: "name"

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

data HyprlandActiveWindow = HyprlandActiveWindow
  { hawAddress :: Text
  } deriving (Show, Eq)

instance FromJSON HyprlandActiveWindow where
  parseJSON = withObject "HyprlandActiveWindow" $ \v ->
    HyprlandActiveWindow <$> v .: "address"

runHyprctlJson :: FromJSON a => [String] -> TaffyIO (Either String a)
runHyprctlJson args = do
  result <- runCommand "hyprctl" args
  return $ case result of
    Left err -> Left err
    Right out ->
      eitherDecode' (BL.pack out)

hyprlandSwitchToWorkspace :: HyprlandWorkspace -> TaffyIO ()
hyprlandSwitchToWorkspace ws = do
  result <- runCommand "hyprctl" ["dispatch", "workspace", workspaceName ws]
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

  clients <- case clientsResult of
    Left err -> wLog WARNING (printf "hyprctl clients failed: %s" err) >> return []
    Right cs -> return cs

  monitors <- case monitorsResult of
    Left err -> wLog WARNING (printf "hyprctl monitors failed: %s" err) >> return []
    Right ms -> return ms

  activeWorkspace <- case activeWorkspaceResult of
    Left err -> wLog WARNING (printf "hyprctl activeworkspace failed: %s" err) >> return Nothing
    Right ws -> return $ Just ws

  buildWorkspacesFromHyprland
    workspaces
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
  -> [HyprlandClient]
  -> [HyprlandMonitorInfo]
  -> Maybe HyprlandWorkspaceRef
  -> Maybe Text
  -> TaffyIO [HyprlandWorkspace]
buildWorkspacesFromHyprland workspaces clients monitors activeWorkspace activeWindowAddress = do
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
          state
            | Just wsId == activeId = Active
            | wsId `elem` visibleIds = Visible
            | null wins = Empty
            | otherwise = Hidden
      in HyprlandWorkspace
           { workspaceIdx = wsId
           , workspaceName = T.unpack wsName
           , workspaceState = state
           , windows = wins
           }

collectWorkspaceWindows :: Maybe Text -> [HyprlandClient] -> M.Map Int [HyprlandWindow]
collectWorkspaceWindows activeWindowAddress =
  foldl' (addWindow activeWindowAddress) M.empty
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
