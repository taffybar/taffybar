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
import           Control.Concurrent (forkIO, killThread)
import           Control.Exception.Enclosed (catchAny)
import           Control.Monad (foldM, forM_, when, (>=>))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson (FromJSON(..), eitherDecode', withObject, (.:), (.:?), (.!=))
import           Data.Char (toLower)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import           Data.Default (Default(..))
import           Data.Int (Int32)
import           Data.List (foldl', sortOn, stripPrefix)
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import           System.Environment (lookupEnv)
import           System.Log.Logger (Priority(..), logM)
import           System.IO (BufferMode(LineBuffering), Handle, IOMode(ReadWriteMode), hClose,
                            hGetLine, hSetBuffering)
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
import           System.Taffybar.Widget.Workspaces
  ( WorkspaceState(..)
  , cssWorkspaceStates
  , getCSSClass
  , possibleStatusStrings
  , updateWidgetClasses
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

data HyprlandWorkspaceWidgetState = HyprlandWorkspaceWidgetState
  { workspaceWidget :: Gtk.Widget
  , workspaceContents :: Gtk.Widget
  , workspaceLabel :: Gtk.Label
  , workspaceIconsBox :: Gtk.Box
  , workspaceLast :: HyprlandWorkspace
  }

newtype HyprlandWorkspaceWidgetCache =
  HyprlandWorkspaceWidgetCache (M.Map Int HyprlandWorkspaceWidgetState)

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
  _ <- liftIO refresh
  tid <- liftIO $ forkIO $ hyprlandUpdateLoop cfg refresh
  _ <- liftIO $ Gtk.onWidgetUnrealize cont $ killThread tid
  Gtk.toWidget cont

hyprlandUpdateLoop :: HyprlandWorkspacesConfig -> IO () -> IO ()
hyprlandUpdateLoop _cfg refresh = do
  mHandle <- connectHyprlandEventSocket
  case mHandle of
    Nothing ->
      logM "System.Taffybar.Widget.HyprlandWorkspaces" WARNING $
        "Hyprland event socket unavailable; workspace updates disabled"
    Just handle ->
      (eventLoop handle `catchAny` \e -> do
          logM "System.Taffybar.Widget.HyprlandWorkspaces" WARNING $
            printf "Hyprland event socket failed (%s); workspace updates disabled" (show e)
          hClose handle)
  where
    eventLoop handle = do
      line <- hGetLine handle
      when (isRelevantHyprEvent line) refresh
      eventLoop handle

connectHyprlandEventSocket :: IO (Maybe Handle)
connectHyprlandEventSocket = do
  mSock <- connectHyprlandSocket ".socket2.sock"
  case mSock of
    Nothing -> return Nothing
    Just sock -> do
      handle <- NS.socketToHandle sock ReadWriteMode
      hSetBuffering handle LineBuffering
      return (Just handle)

connectHyprlandSocket :: FilePath -> IO (Maybe NS.Socket)
connectHyprlandSocket socketName = do
  paths <- hyprlandSocketPaths socketName
  tryPaths paths
  where
    tryPaths [] = return Nothing
    tryPaths (path:rest) = do
      result <- connectToSocket path
      case result of
        Nothing -> tryPaths rest
        Just sock -> return (Just sock)

hyprlandSocketPaths :: FilePath -> IO [FilePath]
hyprlandSocketPaths socketName = do
  mSig <- lookupEnv "HYPRLAND_INSTANCE_SIGNATURE"
  case mSig of
    Nothing -> return []
    Just sig -> do
      mRuntime <- lookupEnv "XDG_RUNTIME_DIR"
      let paths =
            case mRuntime of
              Just runtime -> [runtime ++ "/hypr/" ++ sig ++ "/" ++ socketName]
              Nothing -> []
      return $ paths ++ ["/tmp/hypr/" ++ sig ++ "/" ++ socketName]

connectToSocket :: FilePath -> IO (Maybe NS.Socket)
connectToSocket path =
  (do
      sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
      NS.connect sock (NS.SockAddrUnix path)
      return (Just sock)
    ) `catchAny` \_ -> return Nothing

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
     ]

refreshWorkspaces :: HyprlandWorkspacesConfig -> Gtk.Box -> ReaderT Context IO ()
refreshWorkspaces cfg cont = do
  ws <- getWorkspaces cfg
  HyprlandWorkspaceCache prev <- getStateDefault $ return (HyprlandWorkspaceCache [])
  when (ws /= prev) $ do
    _ <- putState $ return (HyprlandWorkspaceCache ws)
    ctx <- ask
    liftIO $ postGUIASync $ runReaderT (renderWorkspaces cfg cont ws) ctx

renderWorkspaces ::
  HyprlandWorkspacesConfig -> Gtk.Box -> [HyprlandWorkspace] -> ReaderT Context IO ()
renderWorkspaces cfg cont workspaces = do
  let visibleWorkspaces =
        map (applyUrgentState cfg) $ filter (showWorkspaceFn cfg) workspaces
  HyprlandWorkspaceWidgetCache widgetCache <-
    getStateDefault $ return (HyprlandWorkspaceWidgetCache M.empty)
  let buildWidget oldCache (newCache, widgets) ws = do
        let idx = workspaceIdx ws
        state <- case M.lookup idx oldCache of
          Just prevState
            | workspaceLast prevState == ws -> return prevState
            | otherwise -> updateWorkspaceWidgetState cfg ws prevState
          Nothing -> buildWorkspaceWidgetState cfg ws
        let cache' = M.insert idx state newCache
        return (cache', workspaceWidget state : widgets)
  (newCache, widgetsRev) <- foldM
    (buildWidget widgetCache)
    (M.empty, [])
    visibleWorkspaces
  liftIO $ Gtk.containerForeach cont (Gtk.containerRemove cont)
  forM_ (reverse widgetsRev) $ \widget ->
    liftIO $ Gtk.containerAdd cont widget
  liftIO $ Gtk.widgetShowAll cont
  _ <- putState $ return (HyprlandWorkspaceWidgetCache newCache)
  return ()

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
  label <- liftIO $ Gtk.labelNew (Just $ T.pack labelText)
  _ <- widgetSetClassGI label "workspace-label"

  iconsBox <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0
  updateIconsBox cfg ws iconsBox

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
  widget <- Gtk.toWidget ebox
  return HyprlandWorkspaceWidgetState
    { workspaceWidget = widget
    , workspaceContents = contents
    , workspaceLabel = label
    , workspaceIconsBox = iconsBox
    , workspaceLast = ws
    }

updateWorkspaceWidgetState ::
  HyprlandWorkspacesConfig
  -> HyprlandWorkspace
  -> HyprlandWorkspaceWidgetState
  -> ReaderT Context IO HyprlandWorkspaceWidgetState
updateWorkspaceWidgetState cfg ws state = do
  labelText <- labelSetter cfg ws
  liftIO $ Gtk.labelSetText (workspaceLabel state) (T.pack labelText)
  when (windows ws /= windows (workspaceLast state)) $
    updateIconsBox cfg ws (workspaceIconsBox state)
  setWorkspaceWidgetStatusClass ws (workspaceContents state)
  return state { workspaceLast = ws }

updateIconsBox ::
  HyprlandWorkspacesConfig -> HyprlandWorkspace -> Gtk.Box -> ReaderT Context IO ()
updateIconsBox cfg ws iconsBox = do
  liftIO $ Gtk.containerForeach iconsBox (Gtk.containerRemove iconsBox)
  sortedWindows <- iconSort cfg $ windows ws
  let windowCount = length sortedWindows
      maxNeeded = maybe windowCount (min windowCount) (maxIcons cfg)
      shownWindows = map Just $ take maxNeeded sortedWindows
      paddedWindows =
        shownWindows ++ replicate (max 0 (minIcons cfg - length shownWindows)) Nothing
  forM_ paddedWindows $ \windowData -> do
    iconWidget <- buildIconWidget cfg windowData
    liftIO $ Gtk.containerAdd iconsBox iconWidget

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

getWindowStatusString :: HyprlandWindow -> T.Text
getWindowStatusString windowData = T.toLower $ T.pack $
  case windowData of
    HyprlandWindow { windowMinimized = True } -> "minimized"
    HyprlandWindow { windowActive = True } -> show Active
    HyprlandWindow { windowUrgent = True } -> show Urgent
    _ -> "normal"

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
  result <- runHyprlandCommandRaw args
  return $ case result of
    Left err -> Left err
    Right out -> eitherDecode' (BL.fromStrict out)

runHyprlandCommandRaw :: [String] -> TaffyIO (Either String BS.ByteString)
runHyprlandCommandRaw args = do
  let command = hyprlandSocketCommandFromArgs args
  case command of
    Left err -> return (Left err)
    Right cmd -> do
      socketResult <- liftIO $ runHyprlandCommandSocket cmd
      case socketResult of
        Right resp -> return (Right resp)
        Left socketErr -> do
          -- Fallback to hyprctl if the socket is unavailable
          result <- runCommand "hyprctl" args
          return $ case result of
            Left err -> Left $ err ++ " (" ++ socketErr ++ ")"
            Right out ->
              Right $ TE.encodeUtf8 $ T.pack out

hyprlandSocketCommandFromArgs :: [String] -> Either String String
hyprlandSocketCommandFromArgs ("-j":rest) =
  Right $ "j/" ++ unwords rest
hyprlandSocketCommandFromArgs [] =
  Left "No hyprland command provided"
hyprlandSocketCommandFromArgs args =
  Right $ unwords args

runHyprlandCommandSocket :: String -> IO (Either String BS.ByteString)
runHyprlandCommandSocket cmd = do
  mSock <- connectHyprlandSocket ".socket.sock"
  case mSock of
    Nothing -> return (Left "Hyprland command socket unavailable")
    Just sock ->
      (do
          NSB.sendAll sock (BS8.pack cmd)
          NS.shutdown sock NS.ShutdownSend
          resp <- recvAll sock
          NS.close sock
          return (Right resp)
        ) `catchAny` \e -> do
          NS.close sock
          return (Left $ show e)

recvAll :: NS.Socket -> IO BS.ByteString
recvAll sock = go []
  where
    go acc = do
      chunk <- NSB.recv sock 4096
      if BS.null chunk
        then return (BS.concat (reverse acc))
        else go (chunk:acc)

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
