{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Context
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- The "System.Taffybar.Context" module provides the core functionality of the
-- taffybar library. It gets its name from the 'Context' record, which stores
-- runtime information and objects, which are used by many of the widgets that
-- taffybar provides. 'Context' is typically accessed through the 'Reader'
-- interface of 'TaffyIO'.
module System.Taffybar.Context
  ( -- * Configuration
    TaffybarConfig (..),
    defaultTaffybarConfig,
    appendHook,

    -- ** Bars
    BarLevelConfig (..),
    BarConfig (..),
    BarConfigGetter,
    showBarId,

    -- * Taffy monad
    Taffy,
    TaffyIO,

    -- ** Context
    Context (..),
    buildContext,
    buildContextWithBackend,
    buildEmptyContext,

    -- ** Context State
    getState,
    getStateDefault,
    putState,
    setState,

    -- * Control
    refreshTaffyWindows,
    exitTaffybar,

    -- * X11
    runX11,
    runX11Def,

    -- ** Event subscription
    subscribeToAll,
    subscribeToPropertyEvents,
    unsubscribe,

    -- * Threading
    taffyFork,

    -- * Backend (re-exported from "System.Taffybar.Context.Backend")
    Backend (..),
    detectBackend,
  )
where

import Control.Arrow ((&&&), (***))
import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.MVar as MV
import Control.Concurrent.STM.TChan (TChan, readTChan)
import Control.Exception (SomeException, try)
import Control.Exception.Enclosed (catchAny)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified DBus as D
import qualified DBus.Client as DBus
import Data.Char (toLower)
import Data.Data
import Data.Default (Default (..))
import Data.GI.Base (castTo)
import Data.IORef
import Data.Int
import Data.List
import qualified Data.Map as M
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Tuple.Select
import Data.Tuple.Sequence
import Data.Unique
import qualified GI.Gdk
import qualified GI.GdkX11 as GdkX11
import GI.GdkX11.Objects.X11Window
import qualified GI.Gtk as Gtk
import qualified GI.GtkLayerShell as GtkLayerShell
import Graphics.UI.GIGtkStrut
import StatusNotifier.TransparentWindow
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Log.Logger (Priority (..), logM)
import System.Posix.Process (exitImmediately)
import System.Process
  ( CreateProcess (close_fds, new_session, std_err, std_in, std_out),
    StdStream (NoStream),
    createProcess,
    proc,
  )
import System.Taffybar.Context.Backend (Backend (..), detectBackend, prepareBackendEnvironment)
import qualified System.Taffybar.DBus.Client.Params as DBusParams
import System.Taffybar.Information.EWMHDesktopInfo
  ( ewmhActiveWindow,
    ewmhCurrentDesktop,
    getActiveWindow,
    getWindows,
  )
import System.Taffybar.Information.Hyprland
  ( HyprlandClient,
    HyprlandEventChan,
    buildHyprlandEventChan,
    defaultHyprlandClientConfig,
    getFocusedMonitorPosition,
    newHyprlandClient,
    subscribeHyprlandEvents,
  )
import System.Taffybar.Information.SafeX11 hiding (setState)
import System.Taffybar.Information.Wakeup.Manager
  ( WakeupManager,
    newWakeupManager,
  )
import System.Taffybar.Information.X11DesktopInfo
import System.Taffybar.Util
import System.Taffybar.Widget.Util
import System.Taffybar.Window.FocusedMonitor
  ( FocusedMonitorHooks (..),
    setupFocusedMonitorClassUpdates,
  )
import Text.Printf

logIO :: Priority -> String -> IO ()
logIO = logM "System.Taffybar.Context"

logC :: (MonadIO m) => Priority -> String -> m ()
logC p = liftIO . logIO p

-- | 'Taffy' is a monad transformer that provides 'ReaderT' for 'Context'.
type Taffy m v = ReaderT Context m v

-- | 'TaffyIO' is 'IO' wrapped with a 'ReaderT' providing 'Context'. This is the
-- type of most widgets and callback in Taffybar.
type TaffyIO v = ReaderT Context IO v

type Listener = Event -> Taffy IO ()

type SubscriptionList = [(Unique, Listener)]

data Value = forall t. (Typeable t) => Value t

fromValue :: forall t. (Typeable t) => Value -> Maybe t
fromValue (Value v) = cast v

-- | 'BarConfig' specifies the configuration for a single taffybar window.
data BarLevelConfig = BarLevelConfig
  { -- | Constructors for widgets that should be placed at the beginning of the level.
    levelStartWidgets :: [TaffyIO Gtk.Widget],
    -- | Constructors for widgets that should be placed in the center of the level.
    levelCenterWidgets :: [TaffyIO Gtk.Widget],
    -- | Constructors for widgets that should be placed at the end of the level.
    levelEndWidgets :: [TaffyIO Gtk.Widget]
  }

-- | 'BarConfig' specifies the configuration for a single taffybar window.
data BarConfig = BarConfig
  { -- | The strut configuration to use for the bar
    strutConfig :: StrutConfig,
    -- | The amount of spacing in pixels between bar widgets
    widgetSpacing :: Int32,
    -- | Constructors for widgets that should be placed at the beginning of the bar.
    startWidgets :: [TaffyIO Gtk.Widget],
    -- | Constructors for widgets that should be placed in the center of the bar.
    centerWidgets :: [TaffyIO Gtk.Widget],
    -- | Constructors for widgets that should be placed at the end of the bar.
    endWidgets :: [TaffyIO Gtk.Widget],
    -- | Optional level-based widget configuration. If this field is set,
    -- 'startWidgets', 'centerWidgets' and 'endWidgets' are ignored.
    barLevels :: Maybe [BarLevelConfig],
    -- | A unique identifier for the bar, that can be used e.g. when toggling.
    barId :: Unique
  }

instance Eq BarConfig where
  a == b = barId a == barId b

-- | Action that returns the list of bar configurations to realize.
type BarConfigGetter = TaffyIO [BarConfig]

-- | 'TaffybarConfig' provides an advanced interface for configuring taffybar.
-- Through the 'getBarConfigsParam', it is possible to specify different
-- taffybar configurations depending on the number of monitors present, and even
-- to specify different taffybar configurations for each monitor.
data TaffybarConfig = TaffybarConfig
  { -- | An optional dbus client to use.
    dbusClientParam :: Maybe DBus.Client,
    -- | Hooks that should be executed at taffybar startup.
    startupHook :: TaffyIO (),
    -- | A 'TaffyIO' action that returns a list of 'BarConfig' where each element
    -- describes a taffybar window that should be spawned.
    getBarConfigsParam :: BarConfigGetter,
    -- | A list of 'FilePath' each of which should be loaded as css files at
    -- startup.
    cssPaths :: [FilePath],
    -- | A field used (only) by dyre to provide an error message.
    errorMsg :: Maybe String
  }

-- | Append the provided 'TaffyIO' hook to the 'startupHook' of the given
-- 'TaffybarConfig'.
appendHook :: TaffyIO () -> TaffybarConfig -> TaffybarConfig
appendHook hook config =
  config
    { startupHook = startupHook config >> hook
    }

-- | Default values for a 'TaffybarConfig'. Not usuable without at least
-- properly setting 'getBarConfigsParam'.
defaultTaffybarConfig :: TaffybarConfig
defaultTaffybarConfig =
  TaffybarConfig
    { dbusClientParam = Nothing,
      startupHook = return (),
      getBarConfigsParam = return [],
      cssPaths = [],
      errorMsg = Nothing
    }

instance Default TaffybarConfig where
  def = defaultTaffybarConfig

-- | A "Context" value holds all of the state associated with a single running
-- instance of taffybar. It is typically accessed from a widget constructor
-- through the "TaffyIO" monad transformer stack.
data Context = Context
  { -- | The X11Context that will be used to service X11Property requests.
    x11ContextVar :: Maybe (MV.MVar X11Context),
    -- | The handlers which will be evaluated against incoming X11 events.
    listeners :: MV.MVar SubscriptionList,
    -- | A collection of miscellaneous pieces of state which are keyed by their
    -- types. Most new pieces of state should go here, rather than in a new field
    -- in 'Context'. State stored here is typically accessed through
    -- 'getStateDefault'.
    contextState :: MV.MVar (M.Map TypeRep Value),
    -- | Used to track the windows that taffybar is currently controlling, and
    -- which 'BarConfig' objects they are associated with.
    existingWindows :: MV.MVar [(BarConfig, Gtk.Window)],
    -- | The shared user session 'DBus.Client'.
    sessionDBusClient :: DBus.Client,
    -- | The shared system session 'DBus.Client'.
    systemDBusClient :: DBus.Client,
    -- | The action that will be evaluated to get the bar configs associated with
    -- each active monitor taffybar should run on.
    getBarConfigs :: BarConfigGetter,
    -- | A Hyprland client initialized at startup (used by Hyprland widgets).
    --
    -- Stored directly on 'Context' to avoid deadlocks from nested
    -- 'getStateDefault' usage during widget initialization.
    hyprlandClient :: HyprlandClient,
    -- | Lazily-initialized shared Hyprland event channel (socket reader thread).
    --
    -- Stored outside of 'contextState' for the same reason as 'hyprlandClient'.
    hyprlandEventChanVar :: MV.MVar (Maybe HyprlandEventChan),
    -- | Shared wakeup manager for coordinated polling intervals.
    --
    -- Stored outside 'contextState' so wakeup registration cannot deadlock when
    -- called from state initializers that already hold the context-state lock.
    wakeupManager :: WakeupManager,
    -- | The backend taffybar is running on.
    backend :: Backend,
    -- | Populated with the BarConfig that resulted in the creation of a given
    -- widget, when its constructor is called. This lets widgets access thing like
    -- who their neighbors are. Note that the value of 'contextBarConfig' is
    -- different for widgets belonging to bar windows on different monitors.
    contextBarConfig :: Maybe BarConfig
  }

-- | Build the "Context" for a taffybar process.
buildContext :: TaffybarConfig -> IO Context
buildContext cfg = do
  backendType <- detectBackend
  buildContextWithBackend backendType cfg

-- | Build the "Context" for a taffybar process using a pre-detected backend.
--
-- This avoids duplicated backend detection and ensures backend-related
-- environment fixups happen before any downstream initialization that depends
-- on them (e.g. GTK/GDK backend selection).
buildContextWithBackend :: Backend -> TaffybarConfig -> IO Context
buildContextWithBackend
  backendType
  TaffybarConfig
    { dbusClientParam = maybeDBus,
      getBarConfigsParam = barConfigGetter,
      startupHook = startup
    } = do
    logIO DEBUG "Building context"
    dbusC <- maybe DBus.connectSession return maybeDBus
    sDBusC <- DBus.connectSystem
    _ <-
      DBus.requestName
        dbusC
        "org.taffybar.Bar"
        [DBus.nameAllowReplacement, DBus.nameReplaceExisting]
    listenersVar <- MV.newMVar []
    state <- MV.newMVar M.empty
    wakeupMgr <- newWakeupManager
    hyprClient <- newHyprlandClient defaultHyprlandClientConfig
    hyprEventChanVar <- MV.newMVar Nothing
    x11Context <- case backendType of
      BackendX11 -> Just <$> (getX11Context def >>= MV.newMVar)
      BackendWayland -> return Nothing
    windowsVar <- MV.newMVar []
    let context =
          Context
            { x11ContextVar = x11Context,
              listeners = listenersVar,
              contextState = state,
              sessionDBusClient = dbusC,
              systemDBusClient = sDBusC,
              getBarConfigs = barConfigGetter,
              hyprlandClient = hyprClient,
              hyprlandEventChanVar = hyprEventChanVar,
              wakeupManager = wakeupMgr,
              existingWindows = windowsVar,
              backend = backendType,
              contextBarConfig = Nothing
            }
    when (backendType == BackendX11) $
      void $
        runMaybeT $
          MaybeT GI.Gdk.displayGetDefault
            >>= (lift . GI.Gdk.displayGetDefaultScreen)
            >>= ( lift
                    . flip
                      GI.Gdk.afterScreenMonitorsChanged
                      -- XXX: We have to do a force refresh here because there is no
                      -- way to reliably move windows, since the window manager can do
                      -- whatever it pleases.
                      ( runReaderT
                          (forceRefreshTaffyWindowsBecause "gdk screen monitors changed")
                          context
                      )
                )

    -- Some compositors/backends will keep the reserved space for a layer-shell
    -- surface/strut window after suspend, but fail to properly re-display the
    -- window. Listen for systemd-logind resume and force a refresh.
    registerResumeRefresh context

    flip runReaderT context $ do
      logC DEBUG "Starting X11 Handler"
      startX11EventHandler
      logC DEBUG "Running startup hook"
      startup
      logC DEBUG "Queing build windows command"
      refreshTaffyWindows
      when (backendType == BackendWayland) $
        liftIO $
          registerHyprlandReconnectRefresh context
    logIO DEBUG "Context build finished"
    return context

-- | Register a logind sleep/resume listener that forces a window refresh after
-- resume. This is a pragmatic workaround for cases where the bar stays "reserved"
-- (exclusive zone/strut) but stops being visible after resume.
registerResumeRefresh :: Context -> IO ()
registerResumeRefresh ctx = do
  let client = systemDBusClient ctx
      rule =
        DBus.matchAny
          { DBus.matchInterface = Just DBusParams.login1ManagerInterfaceName,
            DBus.matchMember = Just "PrepareForSleep",
            DBus.matchPath = Just DBusParams.login1ObjectPath
          }

  -- Debounce: on some systems we can see multiple resume-related events close
  -- together. Avoid spamming refreshes.
  pendingVar <- MV.newMVar False

  let scheduleRefresh :: IO ()
      scheduleRefresh = do
        wasPending <- MV.swapMVar pendingVar True
        unless wasPending $ void $ forkIO $ do
          -- Give the compositor a moment to re-establish outputs/surfaces.
          threadDelay 1_000_000
          _ <- MV.swapMVar pendingVar False
          logIO NOTICE "Resumed from sleep - forcing taffybar window refresh"
          postGUIASync $
            runReaderT
              (forceRefreshTaffyWindowsBecause "logind resume")
              ctx

      callback :: D.Signal -> IO ()
      callback sig =
        case D.signalBody sig of
          [v] ->
            case D.fromVariant v :: Maybe Bool of
              Just True -> logIO DEBUG "PrepareForSleep(True) received"
              Just False -> scheduleRefresh
              Nothing -> logIO WARNING "PrepareForSleep signal had unexpected body type"
          _ -> logIO WARNING "PrepareForSleep signal had unexpected body arity"

  result <- try (DBus.addMatch client rule callback) :: IO (Either SomeException DBus.SignalHandler)
  case result of
    Left e ->
      logIO WARNING $
        "Failed to register logind PrepareForSleep handler (resume refresh disabled): "
          ++ show e
    Right _ -> pure ()

-- | Recover Wayland bars when the Hyprland event socket reconnects.
--
-- Hyprland restarts can leave existing layer-shell surfaces invisible even
-- though the taffybar process and widget state loops are still alive. The
-- Hyprland event reader emits @taffybar-hyprland-connected@ after every event
-- socket connection, so use reconnects as the compositor-lifecycle signal.
--
-- By default this re-execs taffybar. A full compositor restart can invalidate
-- GTK's Wayland display connection itself, so merely recreating Gtk.Window
-- values in the same process is not reliable. Set
-- @TAFFYBAR_HYPRLAND_RECONNECT_ACTION=refresh@ to keep the older window-only
-- refresh behavior while debugging.
registerHyprlandReconnectRefresh :: Context -> IO ()
registerHyprlandReconnectRefresh ctx = do
  eventChan <- withHyprlandEventChan ctx
  events <- subscribeHyprlandEvents eventChan
  readyVar <- MV.newMVar False
  recoveryPendingVar <- MV.newMVar False
  monitorRefreshPendingVar <- MV.newMVar False
  void $ forkIO $ do
    -- The event reader also emits a connection event during normal startup.
    -- Ignore early connection events so startup does not immediately rebuild
    -- the windows it just created.
    threadDelay 2_000_000
    void $ MV.swapMVar readyVar True

  let lifecycleEventName line =
        let hyprlandEventName = T.takeWhile (/= '>') line
         in if hyprlandEventName
              `elem` [ "taffybar-hyprland-connected",
                       "configreloaded"
                     ]
              then Just hyprlandEventName
              else Nothing

      monitorEventName line =
        let hyprlandEventName = T.takeWhile (/= '>') line
         in if hyprlandEventName
              `elem` [ "monitoradded",
                       "monitorremoved"
                     ]
              then Just hyprlandEventName
              else Nothing

      scheduleProcessRecovery reason = do
        wasPending <- MV.swapMVar recoveryPendingVar True
        unless wasPending $ void $ forkIO $ do
          -- Give Hyprland/GDK a short settling window before rebuilding
          -- layer-shell surfaces.
          threadDelay 1_000_000
          _ <- MV.swapMVar recoveryPendingVar False
          action <- hyprlandReconnectAction
          case action of
            "refresh" -> do
              logIO NOTICE $ "Hyprland " ++ reason ++ " - forcing taffybar window refresh"
              postGUIASync $
                runReaderT
                  (forceRefreshTaffyWindowsBecause $ "hyprland " ++ reason)
                  ctx
            _ -> restartTaffybarProcess ctx reason

      scheduleMonitorRefresh reason = do
        wasPending <- MV.swapMVar monitorRefreshPendingVar True
        unless wasPending $ void $ forkIO $ do
          -- Output topology updates can arrive in small batches; wait briefly
          -- so bar configs are rebuilt against the settled monitor list.
          threadDelay 500_000
          _ <- MV.swapMVar monitorRefreshPendingVar False
          logIO NOTICE $ "Hyprland " ++ reason ++ " - forcing taffybar window refresh"
          postGUIASync $
            runReaderT
              (forceRefreshTaffyWindowsBecause $ "hyprland " ++ reason)
              ctx

      handleLifecycleEvent hyprlandEventName = do
        ready <- MV.readMVar readyVar
        if ready
          then
            if hyprlandEventName == "configreloaded"
              then scheduleProcessRecovery "config reloaded"
              else scheduleProcessRecovery "event socket reconnected"
          else logIO DEBUG "Ignoring Hyprland event socket connection during startup"

      handleMonitorEvent hyprlandEventName = do
        ready <- MV.readMVar readyVar
        if ready
          then scheduleMonitorRefresh $ T.unpack hyprlandEventName
          else logIO DEBUG "Ignoring Hyprland monitor event during startup"

  void $ forkIO $ forever $ do
    line <- atomically $ readTChan events
    maybe (pure ()) handleLifecycleEvent (lifecycleEventName line)
    maybe (pure ()) handleMonitorEvent (monitorEventName line)

hyprlandReconnectAction :: IO String
hyprlandReconnectAction = do
  mAction <- lookupEnv "TAFFYBAR_HYPRLAND_RECONNECT_ACTION"
  pure $ maybe "restart" (map toLower) mAction

restartTaffybarProcess :: Context -> String -> IO ()
restartTaffybarProcess ctx reason = do
  prepareBackendEnvironment
  executable <- getExecutablePath
  args <- getArgs
  logIO NOTICE $
    "Hyprland "
      ++ reason
      ++ " - re-executing taffybar process: "
      ++ executable
  underSystemd <- isSystemdService
  if underSystemd
    then exitImmediately (ExitFailure 75)
    else do
      disconnectClient "session" (sessionDBusClient ctx)
      disconnectClient "system" (systemDBusClient ctx)
      result <- try $ spawnDetachedRestart executable args
      case result of
        Left (e :: SomeException) ->
          logIO WARNING $
            "Failed to restart taffybar process: "
              ++ show e
        Right _ ->
          exitImmediately ExitSuccess
  where
    isSystemdService = maybe False (not . null) <$> lookupEnv "INVOCATION_ID"

    spawnDetachedRestart executable args =
      createProcess
        (proc "sh" (["-c", "sleep 1; exec \"$@\"", "taffybar-restart", executable] ++ args))
          { close_fds = True,
            new_session = True,
            std_in = NoStream,
            std_out = NoStream,
            std_err = NoStream
          }

    disconnectClient name client =
      DBus.disconnect client
        `catchAny` \e ->
          logIO WARNING $
            "Failed to disconnect "
              ++ name
              ++ " DBus client before restart: "
              ++ show e

taffybarWaylandLayer :: IO GtkLayerShell.Layer
taffybarWaylandLayer = do
  mLayer <- lookupEnv "TAFFYBAR_WAYLAND_LAYER"
  case fmap (map toLower) mLayer of
    Just "background" -> pure GtkLayerShell.LayerBackground
    Just "bottom" -> pure GtkLayerShell.LayerBottom
    Just "overlay" -> pure GtkLayerShell.LayerOverlay
    Just "top" -> pure GtkLayerShell.LayerTop
    Just invalid -> do
      logIO WARNING $
        "Invalid TAFFYBAR_WAYLAND_LAYER="
          ++ invalid
          ++ "; using top"
      pure GtkLayerShell.LayerTop
    Nothing -> pure GtkLayerShell.LayerTop

-- | Build an empty taffybar context. This function is mostly useful for
-- invoking functions that yield 'TaffyIO' values in a testing setting (e.g. in
-- a repl).
buildEmptyContext :: IO Context
buildEmptyContext = buildContext def

-- | Format the 'barId' as a numeric string.
showBarId :: BarConfig -> String
showBarId = show . hashUnique . barId

getActiveWindowCenterPoint :: TaffyIO (Maybe (Int, Int))
getActiveWindowCenterPoint = runX11 $ do
  maybeActiveWindow <- getActiveWindow
  allWindows <- getWindows
  case maybeActiveWindow >>= \activeWindow -> activeWindow `guardElem` allWindows of
    Nothing -> return Nothing
    Just activeWindow -> do
      display <- getDisplay
      (_, x, y, width, height, _, _) <- lift $ safeGetGeometry display activeWindow
      let centerX = fromIntegral x + fromIntegral width `div` 2
          centerY = fromIntegral y + fromIntegral height `div` 2
      return $ Just (centerX, centerY)
  where
    guardElem value values =
      if value `elem` values
        then Just value
        else Nothing

getPointerMonitor :: GI.Gdk.Display -> IO (Maybe GI.Gdk.Monitor)
getPointerMonitor display = runMaybeT $ do
  seat <- lift $ GI.Gdk.displayGetDefaultSeat display
  pointer <- MaybeT $ GI.Gdk.seatGetPointer seat
  (_, x, y) <- lift $ GI.Gdk.deviceGetPosition pointer
  MaybeT $ Just <$> GI.Gdk.displayGetMonitorAtPoint display x y

getFocusedMonitorX11 :: Context -> IO (Maybe GI.Gdk.Monitor)
getFocusedMonitorX11 context = runMaybeT $ do
  display <- MaybeT GI.Gdk.displayGetDefault
  maybeCenterPoint <- lift $ runReaderT getActiveWindowCenterPoint context
  case maybeCenterPoint of
    Just (x, y) ->
      MaybeT $
        Just
          <$> GI.Gdk.displayGetMonitorAtPoint
            display
            (fromIntegral x)
            (fromIntegral y)
    Nothing -> MaybeT $ getPointerMonitor display

getFocusedMonitorWayland :: Context -> IO (Maybe GI.Gdk.Monitor)
getFocusedMonitorWayland context = runMaybeT $ do
  display <- MaybeT GI.Gdk.displayGetDefault
  maybeFocusedMonitorPosition <- lift $ getFocusedMonitorPosition (hyprlandClient context)
  case maybeFocusedMonitorPosition of
    Just (x, y) ->
      MaybeT $
        Just
          <$> GI.Gdk.displayGetMonitorAtPoint
            display
            (fromIntegral x)
            (fromIntegral y)
    Nothing -> MaybeT $ getPointerMonitor display

withHyprlandEventChan :: Context -> IO HyprlandEventChan
withHyprlandEventChan context =
  MV.modifyMVar (hyprlandEventChanVar context) $ \existing ->
    case existing of
      Just eventChan -> return (existing, eventChan)
      Nothing -> do
        eventChan <- buildHyprlandEventChan (hyprlandClient context)
        return (Just eventChan, eventChan)

getHyprlandFocusedMonitorEvents :: Context -> IO (TChan T.Text)
getHyprlandFocusedMonitorEvents context = do
  eventChan <- withHyprlandEventChan context
  subscribeHyprlandEvents eventChan

buildBarWindow :: Context -> BarConfig -> IO Gtk.Window
buildBarWindow context barConfig = do
  let thisContext = context {contextBarConfig = Just barConfig}
  logC INFO $
    printf
      "Building window for Taffybar(id=%s) with %s"
      (showBarId barConfig)
      (show $ strutConfig barConfig)

  window <- Gtk.windowNew Gtk.WindowTypeToplevel

  void $ Gtk.onWidgetDestroy window $ do
    let bId = showBarId barConfig
    logC INFO $ printf "Window for Taffybar(id=%s) destroyed" bId
    MV.modifyMVar_ (existingWindows context) (pure . filter ((/=) window . sel2))
    logC DEBUG $ printf "Window for Taffybar(id=%s) unregistered" bId

  setupBarWindow context (strutConfig barConfig) window

  let focusedMonitorHooks =
        case backend thisContext of
          BackendX11 ->
            FocusedMonitorHooksX11
              { resolveFocusedMonitorX11 = getFocusedMonitorX11 thisContext,
                subscribeToFocusedMonitorX11Events =
                  flip runReaderT thisContext
                    . subscribeToPropertyEvents [ewmhActiveWindow, ewmhCurrentDesktop]
                    . const
                    . lift,
                unsubscribeFromFocusedMonitorX11Events =
                  flip runReaderT thisContext . unsubscribe
              }
          BackendWayland ->
            FocusedMonitorHooksWayland
              { resolveFocusedMonitorWayland = getFocusedMonitorWayland thisContext,
                getFocusedMonitorHyprlandEvents = getHyprlandFocusedMonitorEvents thisContext
              }

  _ <- widgetSetClassGI window "taffy-window"
  setupFocusedMonitorClassUpdates
    focusedMonitorHooks
    window
    (strutMonitor (strutConfig barConfig))

  let addWidgetWith widgetAdd (count, buildWidget) =
        runReaderT buildWidget thisContext >>= widgetAdd count

  shownBoxes <-
    case barLevels barConfig of
      Nothing -> do
        box <-
          Gtk.boxNew Gtk.OrientationHorizontal $
            fromIntegral $
              widgetSpacing barConfig
        _ <- widgetSetClassGI box "taffy-box"
        Gtk.widgetSetVexpand box False
        Gtk.setWidgetValign box Gtk.AlignFill
        centerBox <-
          Gtk.boxNew Gtk.OrientationHorizontal $
            fromIntegral $
              widgetSpacing barConfig

        _ <- widgetSetClassGI centerBox "center-box"
        Gtk.widgetSetVexpand centerBox True
        Gtk.setWidgetValign centerBox Gtk.AlignFill
        Gtk.setWidgetHalign centerBox Gtk.AlignCenter
        Gtk.boxSetCenterWidget box (Just centerBox)

        Gtk.containerAdd window box

        let addIndexedClass :: (Gtk.IsWidget b, MonadIO m) => T.Text -> Int -> b -> m b
            addIndexedClass prefix count widget =
              widgetSetClassGI widget $ T.pack $ printf "%s-%d" (T.unpack prefix) count
            addEndPaletteClasses count widget = do
              let _ = count
              widgetSetClassGI widget "end-widget"
            addToStart count widget = do
              _ <- addIndexedClass "left" count widget
              Gtk.boxPackStart box widget False False 0
            addToEnd count widget = do
              _ <- addIndexedClass "right" count widget
              _ <- addEndPaletteClasses count widget
              Gtk.boxPackEnd box widget False False 0
            addToCenter count widget = do
              _ <- addIndexedClass "center" count widget
              Gtk.boxPackStart centerBox widget False False 0

        logIO DEBUG "Building start widgets"
        mapM_ (addWidgetWith addToStart) $ zip [1 ..] (startWidgets barConfig)
        logIO DEBUG "Building center widgets"
        mapM_ (addWidgetWith addToCenter) $ zip [1 ..] (centerWidgets barConfig)
        logIO DEBUG "Building end widgets"
        mapM_ (addWidgetWith addToEnd) $ zip [1 ..] (endWidgets barConfig)

        return [box, centerBox]
      Just levels -> do
        levelsBox <- Gtk.boxNew Gtk.OrientationVertical 0
        _ <- widgetSetClassGI levelsBox "taffy-levels"
        Gtk.widgetSetVexpand levelsBox False
        Gtk.setWidgetValign levelsBox Gtk.AlignFill
        Gtk.containerAdd window levelsBox

        let flattenRows = concatMap (\(box, centerBox) -> [box, centerBox])
            buildLevel (levelCount, BarLevelConfig {levelStartWidgets = starts, levelCenterWidgets = centers, levelEndWidgets = ends}) = do
              box <-
                Gtk.boxNew Gtk.OrientationHorizontal $
                  fromIntegral $
                    widgetSpacing barConfig
              _ <- widgetSetClassGI box "taffy-box"
              _ <- widgetSetClassGI box $ T.pack $ printf "level-%d" (levelCount :: Int)
              Gtk.widgetSetVexpand box False
              Gtk.setWidgetValign box Gtk.AlignFill

              centerBox <-
                Gtk.boxNew Gtk.OrientationHorizontal $
                  fromIntegral $
                    widgetSpacing barConfig
              _ <- widgetSetClassGI centerBox "center-box"
              _ <- widgetSetClassGI centerBox $ T.pack $ printf "level-%d-center" (levelCount :: Int)
              Gtk.widgetSetVexpand centerBox True
              Gtk.setWidgetValign centerBox Gtk.AlignFill
              Gtk.setWidgetHalign centerBox Gtk.AlignCenter
              Gtk.boxSetCenterWidget box (Just centerBox)

              Gtk.boxPackStart levelsBox box False True 0

              let addIndexedClass :: (Gtk.IsWidget b, MonadIO m) => T.Text -> Int -> b -> m b
                  addIndexedClass prefix count widget =
                    widgetSetClassGI widget $ T.pack $ printf "%s-%d" (T.unpack prefix) count
                  addEndPaletteClasses count widget = do
                    let _ = count
                    widgetSetClassGI widget "end-widget"
                  addToStart count widget = do
                    _ <- addIndexedClass "left" count widget
                    Gtk.boxPackStart box widget False False 0
                  addToEnd count widget = do
                    _ <- addIndexedClass "right" count widget
                    _ <- addEndPaletteClasses count widget
                    Gtk.boxPackEnd box widget False False 0
                  addToCenter count widget = do
                    _ <- addIndexedClass "center" count widget
                    Gtk.boxPackStart centerBox widget False False 0

              logIO DEBUG $ printf "Building level %d start widgets" (levelCount :: Int)
              mapM_ (addWidgetWith addToStart) $ zip [1 ..] starts
              logIO DEBUG $ printf "Building level %d center widgets" (levelCount :: Int)
              mapM_ (addWidgetWith addToCenter) $ zip [1 ..] centers
              logIO DEBUG $ printf "Building level %d end widgets" (levelCount :: Int)
              mapM_ (addWidgetWith addToEnd) $ zip [1 ..] ends

              return (box, centerBox)

        levelRows <- mapM buildLevel $ zip [1 ..] levels
        return $ levelsBox : flattenRows levelRows

  makeWindowTransparent window

  logIO DEBUG "Showing window"
  Gtk.widgetShow window
  mapM_ Gtk.widgetShow shownBoxes

  when (backend context == BackendX11) $
    runX11Context context () $ do
      lowered <-
        runMaybeT $ do
          gdkWindow <- MaybeT $ Gtk.widgetGetWindow window
          x11Window <- MaybeT $ liftIO (castTo X11Window gdkWindow)
          xid <- liftIO $ GdkX11.x11WindowGetXid x11Window
          logC DEBUG $ printf "Lowering X11 window %s" $ show xid
          lift $ doLowerWindow (fromIntegral xid)
      when (isNothing lowered) $
        logC WARNING "Skipping X11 lower-window step because the realized GTK window was not an X11Window"

  return window

-- | Use the "barConfigGetter" field of "Context" to get the set of taffybar
-- windows that should active. Will avoid recreating windows if there is already
-- a window with the appropriate geometry and "BarConfig".
refreshTaffyWindows :: TaffyIO ()
refreshTaffyWindows = mapReaderT postGUIASync $ do
  logC DEBUG "Refreshing windows"
  ctx <- ask
  windowsVar <- asks existingWindows

  let rebuildWindows currentWindows = flip runReaderT ctx $
        do
          barConfigs <- join $ asks getBarConfigs

          let currentConfigs = map sel1 currentWindows
              newConfs = filter (`notElem` currentConfigs) barConfigs
              (remainingWindows, removedWindows) =
                partition ((`elem` barConfigs) . sel1) currentWindows
              setPropertiesFromPair (barConf, window) =
                setupBarWindow ctx (strutConfig barConf) window

          newWindowPairs <- lift $ do
            logIO DEBUG $
              printf "removedWindows: %s" $
                show $
                  map (strutConfig . sel1) removedWindows
            logIO DEBUG $
              printf "remainingWindows: %s" $
                show $
                  map (strutConfig . sel1) remainingWindows
            logIO DEBUG $
              printf "newWindows: %s" $
                show $
                  map strutConfig newConfs
            logIO DEBUG $
              printf "barConfigs: %s" $
                show $
                  map strutConfig barConfigs

            -- TODO: This should actually use the config that is provided from
            -- getBarConfigs so that the strut properties of the window can be
            -- altered.
            logIO DEBUG "Updating strut properties for existing windows"
            mapM_ setPropertiesFromPair remainingWindows

            logIO DEBUG "Constructing new windows"
            mapM
              (sequenceT . ((return :: a -> IO a) &&& buildBarWindow ctx))
              newConfs

          return (newWindowPairs ++ remainingWindows, map sel2 removedWindows)

  -- Destroy removed windows AFTER releasing the MVar to avoid deadlock
  -- with the onWidgetDestroy handler, which also modifies existingWindows.
  windowsToDestroy <- lift $ MV.modifyMVar windowsVar rebuildWindows
  lift $ do
    logIO DEBUG "Removing windows"
    mapM_ Gtk.widgetDestroy windowsToDestroy
  logC DEBUG "Finished refreshing windows"
  return ()

-- | Unconditionally delete all existing Taffybar top-level windows.
removeTaffyWindows :: TaffyIO ()
removeTaffyWindows = asks existingWindows >>= liftIO . MV.readMVar >>= deleteWindows
  where
    deleteWindows = mapM_ (sequenceT . (msg *** del))

    msg :: BarConfig -> TaffyIO ()
    msg barConfig =
      logC INFO $
        printf "Destroying window for Taffybar(id=%s)" (showBarId barConfig)

    del :: Gtk.Window -> TaffyIO ()
    del = Gtk.widgetDestroy

-- | Forcibly refresh taffybar windows.
forceRefreshTaffyWindowsBecause :: String -> TaffyIO ()
forceRefreshTaffyWindowsBecause _reason = do
  removeTaffyWindows >> refreshTaffyWindows

-- | Destroys all top-level windows belonging to Taffybar, then
-- requests the GTK main loop to exit.
--
-- This ensures that the windows disappear promptly. For GTK windows
-- to be destroyed, the main loop still needs to be running.
exitTaffybar :: Context -> IO ()
exitTaffybar ctx = do
  postGUIASync $ runReaderT removeTaffyWindows ctx
  Gtk.mainQuit

asksContextVar :: (r -> MV.MVar b) -> ReaderT r IO b
asksContextVar getter = asks getter >>= lift . MV.readMVar

-- | Run a function needing an X11 connection in 'TaffyIO'.
runX11 :: X11Property a -> TaffyIO a
runX11 action = do
  maybeCtxVar <- asks x11ContextVar
  case maybeCtxVar of
    Nothing ->
      liftIO $ fail "X11 context unavailable (Wayland backend in use)"
    Just ctxVar -> do
      ctx <- liftIO $ MV.readMVar ctxVar
      liftIO $ runReaderT action ctx

-- | Use 'runX11' together with 'postX11RequestSyncProp' on the provided
-- property. Return the provided default if 'Nothing' is returned
-- 'postX11RequestSyncProp'.
runX11Def :: a -> X11Property a -> TaffyIO a
runX11Def dflt prop = runX11 $ postX11RequestSyncProp prop dflt

runX11Context :: (MonadIO m) => Context -> a -> X11Property a -> m a
runX11Context context dflt prop =
  liftIO $ runReaderT (runX11Def dflt prop) context

-- | Get a state value by type from the 'contextState' field of 'Context'.
getState :: forall t. (Typeable t) => Taffy IO (Maybe t)
getState = do
  stateMap <- asksContextVar contextState
  let maybeValue = M.lookup (typeRep (Proxy :: Proxy t)) stateMap
  return $ maybeValue >>= fromValue

-- | Like "putState", but avoids aquiring a lock if the value is already in the
-- map.
getStateDefault :: (Typeable t) => Taffy IO t -> Taffy IO t
getStateDefault defaultGetter =
  getState >>= maybe (putState defaultGetter) return

-- | Get a value of the type returned by the provided action from the the
-- current taffybar state, unless the state does not exist, in which case the
-- action will be called to populate the state map.
putState :: forall t. (Typeable t) => Taffy IO t -> Taffy IO t
putState getValue = do
  contextVar <- asks contextState
  ctx <- ask
  lift $ MV.modifyMVar contextVar $ \contextStateMap ->
    let theType = typeRep (Proxy :: Proxy t)
        currentValue = M.lookup theType contextStateMap
        insertAndReturn value =
          (M.insert theType (Value value) contextStateMap, value)
     in flip runReaderT ctx $
          maybe
            (insertAndReturn <$> getValue)
            (return . (contextStateMap,))
            (currentValue >>= fromValue)

-- | Overwrite a state value by type in the 'contextState' field of 'Context'.
-- 'putState'/'getStateDefault' are intentionally "set-once" helpers; widgets
-- that maintain mutable caches should use this to update them.
setState :: forall t. (Typeable t) => t -> Taffy IO t
setState value = do
  contextVar <- asks contextState
  let theType = typeRep (Proxy :: Proxy t)
  lift $ MV.modifyMVar_ contextVar $ \contextStateMap ->
    return $ M.insert theType (Value value) contextStateMap
  return value

-- | A version of 'forkIO' in 'TaffyIO'.
taffyFork :: ReaderT r IO () -> ReaderT r IO ()
taffyFork = void . mapReaderT forkIO

startX11EventHandler :: Taffy IO ()
startX11EventHandler = do
  backendType <- asks backend
  when (backendType == BackendX11) $ taffyFork $ do
    c <- ask
    -- XXX: The event loop needs its own X11Context to separately handle
    -- communications from the X server. We deliberately avoid using the context
    -- from x11ContextVar here.
    lift $
      withX11Context def $
        eventLoop
          (\e -> runReaderT (handleX11Event e) c)

setupBarWindow :: Context -> StrutConfig -> Gtk.Window -> IO ()
setupBarWindow context config window =
  case backend context of
    BackendX11 -> do
      setupStrutWindow config window
      setX11WindowSizeFromStrut config window
    BackendWayland -> setupLayerShellWindow config window

-- | Remove the listener associated with the provided "Unique" from the
-- collection of listeners.
unsubscribe :: Unique -> Taffy IO ()
unsubscribe identifier = do
  listenersVar <- asks listeners
  lift $ MV.modifyMVar_ listenersVar $ return . filter ((== identifier) . fst)

-- | Subscribe to all incoming events on the X11 event loop. The returned
-- "Unique" value can be used to unregister the listener using "unsuscribe".
subscribeToAll :: Listener -> Taffy IO Unique
subscribeToAll listener = do
  identifier <- lift newUnique
  listenersVar <- asks listeners
  let -- XXX: This type annotation probably has something to do with the warnings
      -- that occur without MonoLocalBinds, but it still seems to be necessary
      addListener :: SubscriptionList -> SubscriptionList
      addListener = ((identifier, listener) :)
  lift $ MV.modifyMVar_ listenersVar (return . addListener)
  return identifier

-- | Subscribe to X11 "PropertyEvent"s where the property changed is in the
-- provided list.
subscribeToPropertyEvents :: [String] -> Listener -> Taffy IO Unique
subscribeToPropertyEvents eventNames listener = do
  eventAtoms <- mapM (runX11 . getAtom) eventNames
  let filteredListener event@PropertyEvent {ev_atom = atom} =
        when (atom `elem` eventAtoms) $
          catchAny (listener event) (const $ return ())
      filteredListener _ = return ()
  subscribeToAll filteredListener

handleX11Event :: Event -> Taffy IO ()
handleX11Event event =
  asksContextVar listeners >>= mapM_ applyListener
  where
    applyListener :: (Unique, Listener) -> Taffy IO ()
    applyListener (_, listener) = taffyFork $ listener event

setupLayerShellWindow :: StrutConfig -> Gtk.Window -> IO ()
setupLayerShellWindow
  StrutConfig
    { strutWidth = widthSize,
      strutHeight = heightSize,
      strutXPadding = xpadding,
      strutYPadding = ypadding,
      strutMonitor = monitorNumber,
      strutPosition = position,
      strutDisplayName = maybeDisplayName
    }
  window = do
    supported <- GtkLayerShell.isSupported
    unless supported $
      logIO WARNING "Wayland backend selected, but gtk-layer-shell is not supported"
    when supported $ do
      maybeDisplay <- maybe GI.Gdk.displayGetDefault GI.Gdk.displayOpen maybeDisplayName
      case maybeDisplay of
        Nothing -> logIO WARNING "Failed to get GDK display for layer-shell"
        Just display -> do
          maybeMonitor <-
            maybe
              (GI.Gdk.displayGetPrimaryMonitor display)
              (GI.Gdk.displayGetMonitor display)
              monitorNumber
          case maybeMonitor of
            Nothing -> logIO WARNING "Failed to get GDK monitor for layer-shell"
            Just monitor -> do
              monitorGeometry <- GI.Gdk.monitorGetGeometry monitor
              monitorWidth <- GI.Gdk.getRectangleWidth monitorGeometry
              monitorHeight <- GI.Gdk.getRectangleHeight monitorGeometry
              let width =
                    case widthSize of
                      ExactSize w -> w
                      ScreenRatio p ->
                        floor $ p * fromIntegral (monitorWidth - (2 * xpadding))
                  height =
                    case heightSize of
                      ExactSize h -> h
                      ScreenRatio p ->
                        floor $ p * fromIntegral (monitorHeight - (2 * ypadding))
                  exclusive =
                    case position of
                      TopPos -> height + 2 * ypadding
                      BottomPos -> height + 2 * ypadding
                      LeftPos -> width + 2 * xpadding
                      RightPos -> width + 2 * xpadding

              Gtk.windowSetDefaultSize window (fromIntegral width) (fromIntegral height)
              let (reqWidth, reqHeight) =
                    case position of
                      TopPos -> (-1, height)
                      BottomPos -> (-1, height)
                      LeftPos -> (width, -1)
                      RightPos -> (width, -1)
              Gtk.widgetSetSizeRequest
                window
                (fromIntegral reqWidth)
                (fromIntegral reqHeight)

              alreadyLayer <- GtkLayerShell.isLayerWindow window
              unless alreadyLayer $ do
                GtkLayerShell.initForWindow window
                GtkLayerShell.setKeyboardMode window GtkLayerShell.KeyboardModeNone
                GtkLayerShell.setNamespace window (T.pack "taffybar")
              GtkLayerShell.setMonitor window monitor

              GtkLayerShell.setMargin window GtkLayerShell.EdgeLeft xpadding
              GtkLayerShell.setMargin window GtkLayerShell.EdgeRight xpadding
              GtkLayerShell.setMargin window GtkLayerShell.EdgeTop ypadding
              GtkLayerShell.setMargin window GtkLayerShell.EdgeBottom ypadding

              layer <- taffybarWaylandLayer
              GtkLayerShell.setLayer window layer

              let setAnchor = GtkLayerShell.setAnchor window

              case position of
                TopPos -> do
                  setAnchor GtkLayerShell.EdgeTop True
                  setAnchor GtkLayerShell.EdgeBottom False
                  setAnchor GtkLayerShell.EdgeLeft True
                  setAnchor GtkLayerShell.EdgeRight True
                BottomPos -> do
                  setAnchor GtkLayerShell.EdgeTop False
                  setAnchor GtkLayerShell.EdgeBottom True
                  setAnchor GtkLayerShell.EdgeLeft True
                  setAnchor GtkLayerShell.EdgeRight True
                LeftPos -> do
                  setAnchor GtkLayerShell.EdgeLeft True
                  setAnchor GtkLayerShell.EdgeRight False
                  setAnchor GtkLayerShell.EdgeTop True
                  setAnchor GtkLayerShell.EdgeBottom True
                RightPos -> do
                  setAnchor GtkLayerShell.EdgeLeft False
                  setAnchor GtkLayerShell.EdgeRight True
                  setAnchor GtkLayerShell.EdgeTop True
                  setAnchor GtkLayerShell.EdgeBottom True

              GtkLayerShell.setExclusiveZone window exclusive

              -- Dynamically update exclusive zone when the bar's actual
              -- allocated size exceeds the configured size (issue #270).
              lastExclusiveRef <- newIORef exclusive
              void $ Gtk.onWidgetSizeAllocate window $ \alloc -> do
                allocH <- GI.Gdk.getRectangleHeight alloc
                allocW <- GI.Gdk.getRectangleWidth alloc
                let newExclusive =
                      case position of
                        TopPos -> fromIntegral allocH + 2 * ypadding
                        BottomPos -> fromIntegral allocH + 2 * ypadding
                        LeftPos -> fromIntegral allocW + 2 * xpadding
                        RightPos -> fromIntegral allocW + 2 * xpadding
                prev <- readIORef lastExclusiveRef
                when (newExclusive /= prev) $ do
                  logIO DEBUG $
                    printf "Updating exclusive zone from %d to %d" prev newExclusive
                  writeIORef lastExclusiveRef newExclusive
                  GtkLayerShell.setExclusiveZone window newExclusive

setX11WindowSizeFromStrut :: StrutConfig -> Gtk.Window -> IO ()
setX11WindowSizeFromStrut
  StrutConfig
    { strutWidth = widthSize,
      strutHeight = heightSize,
      strutXPadding = xpadding,
      strutYPadding = ypadding,
      strutMonitor = monitorNumber,
      strutPosition = position,
      strutDisplayName = maybeDisplayName
    }
  window = do
    maybeDisplay <- maybe GI.Gdk.displayGetDefault GI.Gdk.displayOpen maybeDisplayName
    case maybeDisplay of
      Nothing -> pure ()
      Just display -> do
        maybeMonitor <-
          maybe
            (GI.Gdk.displayGetPrimaryMonitor display)
            (GI.Gdk.displayGetMonitor display)
            monitorNumber
        case maybeMonitor of
          Nothing -> pure ()
          Just monitor -> do
            monitorGeometry <- GI.Gdk.monitorGetGeometry monitor
            monitorWidth <- GI.Gdk.getRectangleWidth monitorGeometry
            monitorHeight <- GI.Gdk.getRectangleHeight monitorGeometry
            let width =
                  case widthSize of
                    ExactSize w -> w
                    ScreenRatio p ->
                      floor $ p * fromIntegral (monitorWidth - (2 * xpadding))
                height =
                  case heightSize of
                    ExactSize h -> h
                    ScreenRatio p ->
                      floor $ p * fromIntegral (monitorHeight - (2 * ypadding))
                (reqWidth, reqHeight) =
                  case position of
                    TopPos -> (-1, height)
                    BottomPos -> (-1, height)
                    LeftPos -> (width, -1)
                    RightPos -> (width, -1)
            Gtk.windowSetDefaultSize window (fromIntegral width) (fromIntegral height)
            Gtk.widgetSetSizeRequest
              window
              (fromIntegral reqWidth)
              (fromIntegral reqHeight)
