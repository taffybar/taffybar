{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
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
-----------------------------------------------------------------------------

module System.Taffybar.Context
  ( -- * Configuration
    TaffybarConfig(..)
  , defaultTaffybarConfig
  , appendHook
  -- ** Bars
  , BarConfig(..)
  , BarConfigGetter
  , showBarId

  -- * Taffy monad
  , Taffy
  , TaffyIO
  , runTaffy
  -- ** Context
  , Context(..)
  , buildContext
  , withEmptyContext
  -- ** Context State
  , getState
  , getStateDefault
  , putState

  -- * Control
  , refreshTaffyWindows
  , exitTaffybar

  -- * X11
  , X11PropertyT
  , X11Property
  , runProperty
  , runPropContext

  -- ** Event subscription
  , subscribeToAll
  , subscribeToPropertyEvents
  , unsubscribe

  -- * Threading
  , taffyFork
  , withTaffyThread
  ) where

import           Control.Arrow ((&&&), (***))
import           Control.Monad
import           Control.Monad.IO.Unlift
import           Control.Monad.Managed (MonadManaged(..), defer, managed, managed_, with)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import qualified DBus.Client as DBus
import           Data.Data
import           Data.Default (Default(..))
import           Data.Int
import           Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Tuple.Select
import           Data.Tuple.Sequence
import           Data.Unique
import           Data.GI.Base.Signals (disconnectSignalHandler)
import           GHC.Stack (HasCallStack)
import qualified GI.Gdk
import qualified GI.GdkX11 as GdkX11
import           GI.GdkX11.Objects.X11Window
import qualified GI.Gtk as Gtk
import           Graphics.UI.GIGtkStrut
import           StatusNotifier.TransparentWindow
import           System.Log.Logger (Priority(..), logM)
import           System.Taffybar.Information.X11DesktopInfo (X11Context, X11PropertyT, X11Property, Event(..), withX11EventLoop, doLowerWindow, getAtom, withX11Context)
import           System.Taffybar.Util
import           System.Taffybar.Widget.Util
import           Text.Printf
import           UnliftIO.Concurrent (forkIOWithUnmask, killThread)
import           UnliftIO.Exception (bracket, bracket_, catchAny, uninterruptibleMask_, tryAny, displayException)
import qualified UnliftIO.MVar as MV
import           Unsafe.Coerce

logIO :: Priority -> String -> IO ()
logIO = logM "System.Taffybar.Context"

logC :: MonadIO m => Priority -> String -> m ()
logC p = liftIO . logIO p

-- | 'Taffy' is a monad transformer that provides 'ReaderT' for 'Context'.
type Taffy m v = ReaderT Context m v

-- | 'TaffyIO' is 'IO' wrapped with a 'ReaderT' providing 'Context'. This is the
-- type of most widgets and callback in Taffybar.
type TaffyIO v = ReaderT Context IO v

-- | Apply the provided 'Context' to a 'Taffy' action.
runTaffy :: Context -> Taffy m v -> m v
runTaffy = flip runReaderT

type Listener = Event -> Taffy IO ()
type SubscriptionList = [(Unique, Listener)]
data Value = forall t. Typeable t => Value t

fromValue :: forall t. Typeable t => Value -> Maybe t
fromValue (Value v) =
  if typeOf v == typeRep (Proxy :: Proxy t) then
    Just $ unsafeCoerce v
  else
    Nothing

-- | 'BarConfig' specifies the configuration for a single taffybar window.
data BarConfig = BarConfig
  {
  -- | The strut configuration to use for the bar
    strutConfig :: StrutConfig
  -- | The amount of spacing in pixels between bar widgets
  , widgetSpacing :: Int32
  -- | Constructors for widgets that should be placed at the beginning of the bar.
  , startWidgets :: [TaffyIO Gtk.Widget]
  -- | Constructors for widgets that should be placed in the center of the bar.
  , centerWidgets :: [TaffyIO Gtk.Widget]
  -- | Constructors for widgets that should be placed at the end of the bar.
  , endWidgets :: [TaffyIO Gtk.Widget]
  -- | A unique identifier for the bar, that can be used e.g. when toggling.
  , barId :: Unique
  }

instance Eq BarConfig where
  a == b = barId a == barId b

type BarConfigGetter = TaffyIO [BarConfig]

-- | 'TaffybarConfig' provides an advanced interface for configuring taffybar.
-- Through the 'getBarConfigsParam', it is possible to specify different
-- taffybar configurations depending on the number of monitors present, and even
-- to specify different taffybar configurations for each monitor.
data TaffybarConfig = TaffybarConfig
  {
  -- | An optional dbus client to use.
    dbusClientParam :: Maybe DBus.Client
  -- | Hooks that should be executed at taffybar startup.
  , startupHook :: TaffyIO ()
  -- | A 'TaffyIO' action that returns a list of 'BarConfig' where each element
  -- describes a taffybar window that should be spawned.
  , getBarConfigsParam :: BarConfigGetter
  -- | A list of 'FilePath' each of which should be loaded as css files at
  -- startup.
  , cssPaths :: [FilePath]
  -- | A field used (only) by dyre to provide an error message.
  , errorMsg :: Maybe String
  }


-- | Append the provided 'TaffyIO' hook to the 'startupHook' of the given
-- 'TaffybarConfig'.
appendHook :: TaffyIO () -> TaffybarConfig -> TaffybarConfig
appendHook hook config = config
  { startupHook = startupHook config >> hook }

-- | Default values for a 'TaffybarConfig'. Not usuable without at least
-- properly setting 'getBarConfigsParam'.
defaultTaffybarConfig :: TaffybarConfig
defaultTaffybarConfig = TaffybarConfig
  { dbusClientParam = Nothing
  , startupHook = return ()
  , getBarConfigsParam = return []
  , cssPaths = []
  , errorMsg = Nothing
  }

instance Default TaffybarConfig where
  def = defaultTaffybarConfig

-- | A "Context" value holds all of the state associated with a single running
-- instance of taffybar. It is typically accessed from a widget constructor
-- through the "TaffyIO" monad transformer stack.
data Context = Context
  {
  -- | The X11Context that will be used to service X11Property requests.
    x11ContextVar :: MV.MVar X11Context
  -- | The handlers which will be evaluated against incoming X11 events.
  , listeners :: MV.MVar SubscriptionList
  -- | A collection of miscellaneous pieces of state which are keyed by their
  -- types. Most new pieces of state should go here, rather than in a new field
  -- in 'Context'. State stored here is typically accessed through
  -- 'getStateDefault'.
  , contextState :: MV.MVar (M.Map TypeRep Value)
  -- | Used to track the windows that taffybar is currently controlling, and
  -- which 'BarConfig' objects they are associated with.
  , existingWindows :: MV.MVar [(BarConfig, Gtk.Window)]
  -- | The shared user session 'DBus.Client'.
  , sessionDBusClient :: DBus.Client
  -- | The shared system session 'DBus.Client'.
  , systemDBusClient :: DBus.Client
  -- | The action that will be evaluated to get the bar configs associated with
  -- each active monitor taffybar should run on.
  , getBarConfigs :: BarConfigGetter
  -- | Populated with the BarConfig that resulted in the creation of a given
  -- widget, when its constructor is called. This lets widgets access thing like
  -- who their neighbors are. Note that the value of 'contextBarConfig' is
  -- different for widgets belonging to bar windows on different monitors.
  , contextBarConfig :: Maybe BarConfig
  }

-- | Creates the 'Context' for a Taffybar process.
buildContext :: MonadManaged m => TaffybarConfig -> m Context
buildContext cfg = do
  logC DEBUG "Building context"
  context <- initContext cfg
  runTaffy context (setupContext (startupHook cfg))
  logC DEBUG "Context build finished"
  pure context

-- | Connect to X11 display and DBus; initialize a new 'Context'
-- object for the given config.
initContext :: MonadManaged m => TaffybarConfig -> m Context
initContext cfg = do
  let withSessionBus = bracket DBus.connectSession DBus.disconnect
      withSystemBus = bracket DBus.connectSystem DBus.disconnect
  sessionDBusClient <- maybe (managed withSessionBus) return (dbusClientParam cfg)
  systemDBusClient <- managed withSystemBus
  listeners <- MV.newMVar []
  contextState <- MV.newMVar M.empty
  x11ContextVar <- using getDefaultX11Context >>= MV.newMVar
  windowsVar <- MV.newMVar []
  pure Context
    { x11ContextVar
    , listeners
    , contextState
    , sessionDBusClient
    , systemDBusClient
    , getBarConfigs = getBarConfigsParam cfg
    , existingWindows = windowsVar
    , contextBarConfig = Nothing
    }

-- | Build a new @X11Context@ containing the current X11 display and its root
-- window.
getDefaultX11Context :: MonadManaged m => m X11Context
getDefaultX11Context = managed (withX11Context def)

-- | Using the initial 'Context', this sets up event handlers, runs
-- the startup hook, and schedules the Taffybar UI to be built.
setupContext :: MonadManaged m => Taffy IO () -> Taffy m ()
setupContext onStartup = do
  client <- asks sessionDBusClient
  void . liftIO $ DBus.requestName client "org.taffybar.Bar"
       [DBus.nameAllowReplacement, DBus.nameReplaceExisting]

  logC DEBUG "Starting X11 Event Handler"
  startX11EventHandler

  -- For running Taffy actions within IO callbacks.
  run <- asks runTaffy

  -- Normally the windows are removed by 'exitTaffybar', but we may as
  -- well make sure with this cleanup action.
  defer $ run removeTaffyWindows

  afterMonitorsChanged $ do
    -- We have to do a force refresh here because there is no way to
    -- reliably move windows, since the window manager can do whatever
    -- it pleases.
    logC DEBUG "Monitors configuration changed"
    run forceRefreshTaffyWindows

  mapReaderT liftIO $ do
    logC DEBUG "Running startup hook"
    onStartup
    logC DEBUG "Queing build windows command"
    refreshTaffyWindows

-- | Installs a signal handler to run a callback when the monitors
-- configuration of the default screen changes.
afterMonitorsChanged :: MonadManaged m => IO () -> m ()
afterMonitorsChanged cb = managed_ (withAfterMonitorsChanged cb)

withAfterMonitorsChanged :: MonadUnliftIO m => m () -> m r -> m r
withAfterMonitorsChanged cb action = do
  screen <- mapM GI.Gdk.displayGetDefaultScreen =<< GI.Gdk.displayGetDefault
  maybe id withSignalHandler screen action
  where
    withSignalHandler screen = bracket
      (withRunInIO $ \run -> GI.Gdk.afterScreenMonitorsChanged screen (run cb))
      (liftIO . disconnectSignalHandler screen) . const

-- | Build an empty taffybar context. This function is mostly useful for
-- invoking functions that yield 'TaffyIO' values in a testing setting (e.g. in
-- a repl).
withEmptyContext :: TaffyIO r -> IO r
withEmptyContext = with (buildContext def) . runReaderT

-- | Format the 'barId' as a numeric string.
showBarId :: BarConfig -> String
showBarId = show . hashUnique . barId

buildBarWindow :: Context -> BarConfig -> IO Gtk.Window
buildBarWindow context barConfig = do
  let thisContext = context { contextBarConfig = Just barConfig }
  logC INFO $
      printf "Building window for Taffybar(id=%s) with %s"
      (showBarId barConfig)
      (show $ strutConfig barConfig)

  window <- Gtk.windowNew Gtk.WindowTypeToplevel

  void $ Gtk.onWidgetDestroy window $ do
    let bId = showBarId barConfig
    logC INFO $ printf "Window for Taffybar(id=%s) destroyed" bId
    MV.modifyMVar_ (existingWindows context) (pure . filter ((/=) window . sel2))
    logC DEBUG $ printf "Window for Taffybar(id=%s) unregistered" bId

  box <- Gtk.boxNew Gtk.OrientationHorizontal $ fromIntegral $
         widgetSpacing barConfig
  _ <- widgetSetClassGI box "taffy-box"
  centerBox <- Gtk.boxNew Gtk.OrientationHorizontal $
               fromIntegral $ widgetSpacing barConfig

  _ <- widgetSetClassGI centerBox "center-box"
  Gtk.widgetSetVexpand centerBox True
  Gtk.setWidgetValign centerBox Gtk.AlignFill
  Gtk.setWidgetHalign centerBox Gtk.AlignCenter
  Gtk.boxSetCenterWidget box (Just centerBox)

  setupStrutWindow (strutConfig barConfig) window
  Gtk.containerAdd window box

  _ <- widgetSetClassGI window "taffy-window"

  let addWidgetWith widgetAdd (count, buildWidget) =
        runTaffy thisContext buildWidget >>= widgetAdd count
      addToStart count widget = do
        _ <- widgetSetClassGI widget $ T.pack $ printf "left-%d" (count :: Int)
        Gtk.boxPackStart box widget False False 0
      addToEnd count widget = do
        _ <- widgetSetClassGI widget $ T.pack $ printf "right-%d" (count :: Int)
        Gtk.boxPackEnd box widget False False 0
      addToCenter count widget = do
        _ <- widgetSetClassGI widget $ T.pack $ printf "center-%d" (count :: Int)
        Gtk.boxPackStart centerBox widget False False 0

  logIO DEBUG "Building start widgets"
  mapM_ (addWidgetWith addToStart) $ zip [1..] (startWidgets barConfig)
  logIO DEBUG "Building center widgets"
  mapM_ (addWidgetWith addToCenter) $ zip [1..] (centerWidgets barConfig)
  logIO DEBUG "Building end widgets"
  mapM_ (addWidgetWith addToEnd) $ zip [1..] (endWidgets barConfig)

  makeWindowTransparent window

  logIO DEBUG "Showing window"
  Gtk.widgetShow window
  logIO DEBUG "Shown window"
  Gtk.widgetShow box
  Gtk.widgetShow centerBox

  lowerX11Window context window

  return window

lowerX11Window :: (HasCallStack, MonadUnliftIO m) => Context -> Gtk.Window -> m ()
lowerX11Window context window = void $ runMaybeT $ do
    gdkWindow <- MaybeT $ Gtk.widgetGetWindow window
    x11Window <- MaybeT $ liftIO $ Gtk.castTo X11Window gdkWindow
    xid <- fromIntegral <$> GdkX11.x11WindowGetXid x11Window
    logC DEBUG $ printf "Lowering X11 window 0x%x" xid
    lift $ runPropContext context $ doLowerWindow xid

-- | Use the "barConfigGetter" field of "Context" to get the set of taffybar
-- windows that should active. Will avoid recreating windows if there is already
-- a window with the appropriate geometry and "BarConfig".
refreshTaffyWindows :: TaffyIO ()
refreshTaffyWindows = mapReaderT postGUIASync $ do
  logC DEBUG "Refreshing windows"
  windowsVar <- asks existingWindows

  let rebuildWindows :: [(BarConfig, Gtk.Window)] -> TaffyIO [(BarConfig, Gtk.Window)]
      rebuildWindows currentWindows = do
          barConfigs <- join $ asks getBarConfigs

          let currentConfigs = map sel1 currentWindows
              newConfs = filter (`notElem` currentConfigs) barConfigs
              (remainingWindows, removedWindows) =
                partition ((`elem` barConfigs) . sel1) currentWindows
              setPropertiesFromPair (barConf, window) =
                setupStrutWindow (strutConfig barConf) window

          newWindowPairs <- do
            logC DEBUG $ printf "removedWindows: %s" $
                  show $ map (strutConfig . sel1) removedWindows
            logC DEBUG $ printf "remainingWindows: %s" $
                  show $ map (strutConfig . sel1) remainingWindows
            logC DEBUG $ printf "newWindows: %s" $
                  show $ map strutConfig newConfs
            logC DEBUG $ printf "barConfigs: %s" $
                  show $ map strutConfig barConfigs

            logC DEBUG "Removing windows"
            mapM_ (Gtk.widgetDestroy . sel2) removedWindows

            -- TODO: This should actually use the config that is provided from
            -- getBarConfigs so that the strut properties of the window can be
            -- altered.
            logC DEBUG "Updating strut properties for existing windows"
            mapM_ setPropertiesFromPair remainingWindows

            logC DEBUG "Constructing new windows"
            ctx <- ask
            lift $ mapM (sequenceT . (return @IO &&& buildBarWindow ctx))
                 newConfs

          return $ newWindowPairs ++ remainingWindows

  MV.modifyMVar_ windowsVar rebuildWindows
  logC DEBUG "Finished refreshing windows"

-- | Unconditionally delete all existing Taffybar top-level windows.
removeTaffyWindows :: TaffyIO ()
removeTaffyWindows = asks existingWindows >>= MV.readMVar >>= deleteWindows
  where
    deleteWindows = mapM_ (sequenceT . (msg *** del))

    msg :: BarConfig -> Taffy IO ()
    msg barConfig = logC INFO $
      printf "Destroying window for Taffybar(id=%s)" (showBarId barConfig)

    del :: Gtk.Window -> Taffy IO ()
    del = Gtk.widgetDestroy

-- | Forcibly refresh taffybar windows, even if there are existing windows that
-- correspond to the uniques in the bar configs yielded by 'barConfigGetter'.
forceRefreshTaffyWindows :: TaffyIO ()
forceRefreshTaffyWindows = removeTaffyWindows >> refreshTaffyWindows

-- | Destroys all top-level windows belonging to Taffybar, then
-- requests the GTK main loop to exit.
--
-- This ensures that the windows disappear promptly. For GTK windows
-- to be destroyed, the main loop still needs to be running.
exitTaffybar :: Context -> IO ()
exitTaffybar ctx = do
  postGUIASync $ runTaffy ctx removeTaffyWindows
  Gtk.mainQuit

asksContextVar :: MonadIO m => (r -> MV.MVar b) -> ReaderT r m b
asksContextVar getter = asks getter >>= MV.readMVar

-- | Run a function needing an X11 connection in 'TaffyIO'.
runProperty :: MonadIO m => X11PropertyT m a -> Taffy m a
runProperty action = asksContextVar x11ContextVar >>= lift . runReaderT action

runPropContext :: (HasCallStack, MonadUnliftIO m) => Context -> X11PropertyT m a -> m a
runPropContext context = runTaffy context . runProperty

-- | Get a state value by type from the 'contextState' field of 'Context'.
getState :: forall t. Typeable t => Taffy IO (Maybe t)
getState = do
  stateMap <- asksContextVar contextState
  let maybeValue = M.lookup (typeRep (Proxy :: Proxy t)) stateMap
  return $ maybeValue >>= fromValue

-- | Like "putState", but avoids aquiring a lock if the value is already in the
-- map.
getStateDefault :: Typeable t => Taffy IO t -> Taffy IO t
getStateDefault defaultGetter =
  getState >>= maybe (putState defaultGetter) return

-- | Get a value of the type returned by the provided action from the the
-- current taffybar state, unless the state does not exist, in which case the
-- action will be called to populate the state map.
putState :: forall t. Typeable t => Taffy IO t -> Taffy IO t
putState getValue = do
  contextVar <- asks contextState
  MV.modifyMVar contextVar $ \contextStateMap ->
    let theType = typeRep (Proxy :: Proxy t)
        currentValue = M.lookup theType contextStateMap
        insertAndReturn value =
          (M.insert theType (Value value) contextStateMap, value)
    in maybe
         (insertAndReturn  <$> getValue)
         (return . (contextStateMap,))
         (currentValue >>= fromValue)

-- | A version of 'forkIO' for 'TaffyIO' actions. The result is a
-- 'Managed' value, to avoid unintentionally leaving the thread
-- running.
taffyFork :: MonadManaged m => String -> ReaderT r IO () -> ReaderT r m ()
taffyFork name action = managedR_ $ withTaffyThread name action

-- | Runs a 'forkIO' thread with the first given 'IO' action, only
-- while the main 'IO' action is running.
--
-- When the main action exits, the thread will be killed.
--
-- If the thread terminates early, this will be logged, but the main
-- action will continue to run.
withTaffyThread
  :: MonadUnliftIO m
  => String  -- ^ Name of thread (for logging purposes)
  -> m () -- ^ Action to run in another thread
  -> m c -- ^ Main action
  -> m c
withTaffyThread name action = bracket (fork action) kill . const
  where
    -- NB. 'bracket' runs the setup action with async exceptions
    -- masked, forked threads will inherit this mask, so we need to
    -- unmask while running the action.
    fork f = forkIOWithUnmask (\unmask -> labelMyThread name >> unmask (logFork f))
    -- NB. unliftio already applies 'uninterruptibleMask_' but it
    -- can't hurt to make sure the cleanup action can't be cancelled.
    kill = logKill . uninterruptibleMask_ . killThread

    logFork f = do
      logThread "starting"
      tryAny f >>= \case
        Right () -> logThread "finished normally"
        Left e -> logThread' ERROR ("Unhandled exception: " ++ displayException e)

    logKill = bracket_
        (logThread "stopping (if necessary)")
        (logThread "stopped")

    logThread' p msg = logC p (printf "Thread %s: %s" name msg)
    logThread = logThread' DEBUG

-- | Fork an event loop thread to handle X11 events. Events will be
-- dispatched synchronously by 'handleX11Event' in the event loop
-- thread.
--
-- NB: The event loop needs its own 'X11Context' to separately handle
-- communications from the X server. We deliberately avoid using the context
-- from 'x11ContextVar' here.
startX11EventHandler :: MonadManaged m => Taffy m ()
startX11EventHandler = do
  run <- asks runTaffy
  lift . managed_ $ withX11EventLoop def (run . handleX11Event)

handleX11Event :: Event -> Taffy IO ()
handleX11Event event =
  asksContextVar listeners >>= mapM_ applyListener
  where applyListener :: (Unique, Listener) -> Taffy IO ()
        applyListener (_, listener) = listener event

-- | Remove the listener associated with the provided "Unique" from the
-- collection of listeners.
unsubscribe :: Unique -> Taffy IO ()
unsubscribe identifier = do
  listenersVar <- asks listeners
  MV.modifyMVar_ listenersVar $ return . filter ((== identifier) . fst)

-- | Subscribe to all incoming events on the X11 event loop. The returned
-- "Unique" value can be used to unregister the listener using "unsuscribe".
subscribeToAll :: Listener -> Taffy IO Unique
subscribeToAll listener = do
  identifier <- lift newUnique
  listenersVar <- asks listeners
  let
    -- XXX: This type annotation probably has something to do with the warnings
    -- that occur without MonoLocalBinds, but it still seems to be necessary
    addListener :: SubscriptionList -> SubscriptionList
    addListener = ((identifier, listener):)
  MV.modifyMVar_ listenersVar (return . addListener)
  return identifier

-- | Subscribe to X11 "PropertyEvent"s where the property changed is in the
-- provided list.
subscribeToPropertyEvents :: [String] -> Listener -> Taffy IO Unique
subscribeToPropertyEvents eventNames listener = do
  eventAtoms <- mapM (runProperty . getAtom) eventNames
  let filteredListener event@PropertyEvent { ev_atom = atom } =
        when (atom `elem` eventAtoms) $
             catchAny (listener event) (const $ return ())
      filteredListener _ = return ()
  subscribeToAll filteredListener
