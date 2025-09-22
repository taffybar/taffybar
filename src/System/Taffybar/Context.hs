{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
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
  -- ** Context
  , Context(..)
  , buildContext
  , buildEmptyContext
  -- ** Context State
  , getState
  , getStateDefault
  , putState

  -- * Control
  , refreshTaffyWindows
  , exitTaffybar

  -- * X11
  , runX11
  , runX11Def
  -- ** Event subscription
  , subscribeToAll
  , subscribeToPropertyEvents
  , unsubscribe

  -- * Threading
  , taffyFork
  ) where

import           Control.Arrow ((&&&), (***))
import           Control.Concurrent (forkIO)
import qualified Control.Concurrent.MVar as MV
import           Control.Exception.Enclosed (catchAny)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.RateLimit
import qualified DBus.Client as DBus
import           Data.Data
import           Data.Default (Default(..))
import           Data.GI.Base.ManagedPtr (unsafeCastTo)
import           Data.Int
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time.Units
import           Data.Tuple.Select
import           Data.Tuple.Sequence
import           Data.Unique
import qualified GI.Gdk
import qualified GI.GdkX11 as GdkX11
import           GI.GdkX11.Objects.X11Window
import qualified GI.Gtk as Gtk
import           Graphics.UI.GIGtkStrut
import           StatusNotifier.TransparentWindow
import           System.Log.Logger (Priority(..), logM)
import           System.Taffybar.Information.EWMHDesktopInfo (ewmhCurrentDesktop)
import           System.Taffybar.Information.SafeX11
import           System.Taffybar.Information.X11DesktopInfo
import           System.Taffybar.Util
import           System.Taffybar.Widget.Util
import           Text.Printf
import           Text.Read (readMaybe)
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

-- | Build the "Context" for a taffybar process.
buildContext :: TaffybarConfig -> IO Context
buildContext TaffybarConfig
               { dbusClientParam = maybeDBus
               , getBarConfigsParam = barConfigGetter
               , startupHook = startup
               } = do
  logIO DEBUG "Building context"
  dbusC <- maybe DBus.connectSession return maybeDBus
  sDBusC <- DBus.connectSystem
  _ <- DBus.requestName dbusC "org.taffybar.Bar"
       [DBus.nameAllowReplacement, DBus.nameReplaceExisting]
  listenersVar <- MV.newMVar []
  state <- MV.newMVar M.empty
  x11Context <- getX11Context def >>= MV.newMVar
  windowsVar <- MV.newMVar []
  let context = Context
                { x11ContextVar = x11Context
                , listeners = listenersVar
                , contextState = state
                , sessionDBusClient = dbusC
                , systemDBusClient = sDBusC
                , getBarConfigs = barConfigGetter
                , existingWindows = windowsVar
                , contextBarConfig = Nothing
                }
  _ <- runMaybeT $ MaybeT GI.Gdk.displayGetDefault >>=
              (lift . GI.Gdk.displayGetDefaultScreen) >>=
              (lift . flip GI.Gdk.afterScreenMonitorsChanged
               -- XXX: We have to do a force refresh here because there is no
               -- way to reliably move windows, since the window manager can do
               -- whatever it pleases.
               (runReaderT forceRefreshTaffyWindows context))
  flip runReaderT context $ do
    logC DEBUG "Starting X11 Handler"
    startX11EventHandler
    logC DEBUG "Running startup hook"
    startup
    logC DEBUG "Queing build windows command"
    refreshTaffyWindows
  logIO DEBUG "Context build finished"
  return context

-- | Build an empty taffybar context. This function is mostly useful for
-- invoking functions that yield 'TaffyIO' values in a testing setting (e.g. in
-- a repl).
buildEmptyContext :: IO Context
buildEmptyContext = buildContext def

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

  logIO DEBUG "Subscribing to ewmhCurrentDesktop changes"
  updateHandler <- onCurrentDesktopUpdate window thisContext
  let doUpdate = lift . updateHandler
  currentDesktopSubscription <- flip runReaderT thisContext $
    subscribeToPropertyEvents [ewmhCurrentDesktop] $ doUpdate
  let doUnsubscribe = flip runReaderT thisContext $
        unsubscribe currentDesktopSubscription
  _ <- Gtk.onWidgetUnrealize window doUnsubscribe

  let addWidgetWith widgetAdd (count, buildWidget) =
        runReaderT buildWidget thisContext >>= widgetAdd count
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
  Gtk.widgetShow box
  Gtk.widgetShow centerBox

  runX11Context context () $ void $ runMaybeT $ do
    gdkWindow <- MaybeT $ Gtk.widgetGetWindow window
    xid <- GdkX11.x11WindowGetXid =<< liftIO (unsafeCastTo X11Window gdkWindow)
    logC DEBUG $ printf "Lowering X11 window %s" $ show xid
    lift $ doLowerWindow (fromIntegral xid)

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
                setupStrutWindow (strutConfig barConf) window

          newWindowPairs <- lift $ do
            logIO DEBUG $ printf "removedWindows: %s" $
                  show $ map (strutConfig . sel1) removedWindows
            logIO DEBUG $ printf "remainingWindows: %s" $
                  show $ map (strutConfig . sel1) remainingWindows
            logIO DEBUG $ printf "newWindows: %s" $
                  show $ map strutConfig newConfs
            logIO DEBUG $ printf "barConfigs: %s" $
                  show $ map strutConfig barConfigs

            logIO DEBUG "Removing windows"
            mapM_ (Gtk.widgetDestroy . sel2) removedWindows

            -- TODO: This should actually use the config that is provided from
            -- getBarConfigs so that the strut properties of the window can be
            -- altered.
            logIO DEBUG "Updating strut properties for existing windows"
            mapM_ setPropertiesFromPair remainingWindows

            logIO DEBUG "Constructing new windows"
            mapM (sequenceT . ((return :: a -> IO a) &&& buildBarWindow ctx))
                 newConfs

          return $ newWindowPairs ++ remainingWindows

  lift $ MV.modifyMVar_ windowsVar rebuildWindows
  logC DEBUG "Finished refreshing windows"
  return ()

-- | Unconditionally delete all existing Taffybar top-level windows.
removeTaffyWindows :: TaffyIO ()
removeTaffyWindows = asks existingWindows >>= liftIO . MV.readMVar >>= deleteWindows
  where
    deleteWindows = mapM_ (sequenceT . (msg *** del))

    msg :: BarConfig -> TaffyIO ()
    msg barConfig = logC INFO $
      printf "Destroying window for Taffybar(id=%s)" (showBarId barConfig)

    del :: Gtk.Window -> TaffyIO ()
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
  postGUIASync $ runReaderT removeTaffyWindows ctx
  Gtk.mainQuit

rateLimitFn
  :: forall req resp.
     Context
  -> (req -> IO resp)
  -> ResultsCombiner req resp
  -> IO (req -> IO resp)
rateLimitFn context =
  let limit = 100000
      -- TODO: make configurable, not hardcoded
      -- something like: (updateRateLimitMicroseconds $ workspacesConfig context)
      rate = fromMicroseconds limit :: Microsecond in
  generateRateLimitedFunction $ PerInvocation rate

onCurrentDesktopUpdate :: Gtk.Window -> Context -> IO (Event -> IO ())
onCurrentDesktopUpdate window context = do
  rateLimited <- rateLimitFn context doUpdate combineRequests
  let withLog event = do
        case event of
          PropertyEvent _ _ _ _ _ atom _ _ ->
            logIO DEBUG $ printf "Event %s" $ show atom
          _ -> return ()
        void $ forkIO $ rateLimited event
  return withLog
  where
    combineRequests _ b = Just (b, const ((), ()))
    doUpdate _ = postGUIASync $ flip runReaderT context $ updateTaffyWindowStatus window

updateTaffyWindowStatus :: Gtk.Window -> TaffyIO ()
updateTaffyWindowStatus window = do
    ctx <- ask
    runX11Context ctx () $ void $ do
        barXY <- Gtk.windowGetPosition window
        let barX = fromIntegral $ fst barXY
            barY = fromIntegral $ snd barXY
        logC DEBUG $ printf "taffybar window position: %d %d" barX barY

        fmRectList <- getFocusedMonitorRect
        let fmRectStr = case fmRectList of (x:xs) -> x
                                           []     -> "0 0 1 1" -- default: top left pixel
            fmRectWords = words fmRectStr
            readMaybeInt s = readMaybe s :: Maybe Int
            [monX, monY, monW, monH]
                | (length fmRectWords) == 4 = map ((fromMaybe 0) . readMaybeInt) fmRectWords
                | otherwise                 = [0, 0, 1, 1] -- default: top left pixel

        logC DEBUG $ printf "_XMONAD_FOCUSED_MONITOR_GEOMETRY: %d %d %d %d" monX monY monW monH

        let monFocusStatus
                | (monX < barX) && (barX < monX + monW) &&
                  (monY < barY) && (barY < monY + monH)     = "focused-monitor"
                | otherwise                                 = "unfocused-monitor"

        updateWidgetClasses window
                            [monFocusStatus]
                            ["focused-monitor", "unfocused-monitor"]

    return ()

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

asksContextVar :: (r -> MV.MVar b) -> ReaderT r IO b
asksContextVar getter = asks getter >>= lift . MV.readMVar

-- | Run a function needing an X11 connection in 'TaffyIO'.
runX11 :: X11Property a -> TaffyIO a
runX11 action =
  asksContextVar x11ContextVar >>= lift . runReaderT action

-- | Use 'runX11' together with 'postX11RequestSyncProp' on the provided
-- property. Return the provided default if 'Nothing' is returned
-- 'postX11RequestSyncProp'.
runX11Def :: a -> X11Property a -> TaffyIO a
runX11Def dflt prop = runX11 $ postX11RequestSyncProp prop dflt

runX11Context :: MonadIO m => Context -> a -> X11Property a -> m a
runX11Context context dflt prop =
  liftIO $ runReaderT (runX11Def dflt prop) context

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
  ctx <- ask
  lift $ MV.modifyMVar contextVar $ \contextStateMap ->
    let theType = typeRep (Proxy :: Proxy t)
        currentValue = M.lookup theType contextStateMap
        insertAndReturn value =
          (M.insert theType (Value value) contextStateMap, value)
    in flip runReaderT ctx $  maybe
         (insertAndReturn  <$> getValue)
         (return . (contextStateMap,))
         (currentValue >>= fromValue)

-- | A version of 'forkIO' in 'TaffyIO'.
taffyFork :: ReaderT r IO () -> ReaderT r IO ()
taffyFork = void . mapReaderT forkIO

startX11EventHandler :: Taffy IO ()
startX11EventHandler = taffyFork $ do
  c <- ask
  -- XXX: The event loop needs its own X11Context to separately handle
  -- communications from the X server. We deliberately avoid using the context
  -- from x11ContextVar here.
  lift $ withX11Context def $ eventLoop
         (\e -> runReaderT (handleX11Event e) c)

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
  let
    -- XXX: This type annotation probably has something to do with the warnings
    -- that occur without MonoLocalBinds, but it still seems to be necessary
    addListener :: SubscriptionList -> SubscriptionList
    addListener = ((identifier, listener):)
  lift $ MV.modifyMVar_ listenersVar (return . addListener)
  return identifier

-- | Subscribe to X11 "PropertyEvent"s where the property changed is in the
-- provided list.
subscribeToPropertyEvents :: [String] -> Listener -> Taffy IO Unique
subscribeToPropertyEvents eventNames listener = do
  eventAtoms <- mapM (runX11 . getAtom) eventNames
  let filteredListener event@PropertyEvent { ev_atom = atom } =
        when (atom `elem` eventAtoms) $
             catchAny (listener event) (const $ return ())
      filteredListener _ = return ()
  subscribeToAll filteredListener

handleX11Event :: Event -> Taffy IO ()
handleX11Event event =
  asksContextVar listeners >>= mapM_ applyListener
  where applyListener :: (Unique, Listener) -> Taffy IO ()
        applyListener (_, listener) = taffyFork $ listener event
