{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
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
-----------------------------------------------------------------------------

module System.Taffybar.Context where

import           Control.Arrow ((&&&))
import           Control.Concurrent (forkIO)
import qualified Control.Concurrent.MVar as MV
import           Control.Exception.Enclosed (catchAny)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import qualified DBus.Client as DBus
import           Data.Data
import           Data.GI.Base.ManagedPtr (unsafeCastTo)
import           Data.Int
import           Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Tuple.Select
import           Data.Tuple.Sequence
import           Data.Unique
import qualified GI.Gdk
import qualified GI.GdkX11 as GdkX11
import           GI.GdkX11.Objects.X11Window
import qualified GI.Gtk as Gtk
import           Graphics.UI.GIGtkStrut
import           StatusNotifier.TransparentWindow
import           System.Log.Logger
import           System.Taffybar.Information.SafeX11
import           System.Taffybar.Information.X11DesktopInfo
import           System.Taffybar.Util
import           System.Taffybar.Widget.Util
import           Text.Printf
import           Unsafe.Coerce

logIO :: System.Log.Logger.Priority -> String -> IO ()
logIO = logM "System.Taffybar.Context"

logC :: MonadIO m => System.Log.Logger.Priority -> String -> m ()
logC p = liftIO . logIO p

type Taffy m v = MonadIO m => ReaderT Context m v
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

data BarConfig = BarConfig
  { strutConfig :: StrutConfig
  , widgetSpacing :: Int32
  , startWidgets :: [TaffyIO Gtk.Widget]
  , centerWidgets :: [TaffyIO Gtk.Widget]
  , endWidgets :: [TaffyIO Gtk.Widget]
  , barId :: Unique
  }

instance Eq BarConfig where
  a == b = barId a == barId b

type BarConfigGetter = TaffyIO [BarConfig]

data TaffybarConfig = TaffybarConfig
  { dbusClientParam :: Maybe DBus.Client
  , startupHook :: TaffyIO ()
  , getBarConfigsParam :: BarConfigGetter
  , cssPaths :: [FilePath]
  , errorMsg :: Maybe String
  }

appendHook :: TaffyIO () -> TaffybarConfig -> TaffybarConfig
appendHook hook config = config
  { startupHook = startupHook config >> hook }

defaultTaffybarConfig :: TaffybarConfig
defaultTaffybarConfig = TaffybarConfig
  { dbusClientParam = Nothing
  , startupHook = return ()
  , getBarConfigsParam = return []
  , cssPaths = []
  , errorMsg = Nothing
  }

-- | A "Context" value holds all of the state associated with a single running
-- instance of taffybar. It is typically accessed from a widget constructor
-- through the "TaffyIO" monad transformer stack.
data Context = Context
  {
  -- | The X11Context that will be used to service X11Property requests.
    x11ContextVar :: MV.MVar X11Context
  -- | The handlers which will be evaluated against incoming X11 events.
  , listeners :: MV.MVar SubscriptionList
  -- | A collection of miscellaneous peices of state which are keyed by their
  -- types. Most new peices of state should go here, rather than in a new field
  -- in "Context". State stored here is typically accessed through
  -- "getStateDefault".
  , contextState :: MV.MVar (M.Map TypeRep Value)
  -- | Used to track the windows that taffybar is currently controlling, and
  -- which "BarConfig" objects they are associated with.
  , existingWindows :: MV.MVar [(BarConfig, Gtk.Window)]
  -- | The shared user session "DBus.Client".
  , sessionDBusClient :: DBus.Client
  -- | The shared system session "DBus.Client".
  , systemDBusClient :: DBus.Client
  -- | The action that will be evaluated to get the bar configs associated with
  -- each active monitor taffybar should run on.
  , getBarConfigs :: BarConfigGetter
  -- | Populated with the BarConfig that resulted in the creation of a given
  -- widget, when its constructor is called. This lets widgets access thing like
  -- who their neighbors are. Note that the value of "contextBarConfig" is
  -- different for widgets belonging to bar windows on differnt monitors.
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
  x11Context <- getDefaultCtx >>= MV.newMVar
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

buildEmptyContext :: IO Context
buildEmptyContext = buildContext defaultTaffybarConfig

buildBarWindow :: Context -> BarConfig -> IO Gtk.Window
buildBarWindow context barConfig = do
  let thisContext = context { contextBarConfig = Just barConfig }
  logIO DEBUG $
      printf "Building bar window with StrutConfig: %s" $
      show $ strutConfig barConfig

  window <- Gtk.windowNew Gtk.WindowTypeToplevel
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
refreshTaffyWindows = liftReader postGUIASync $ do
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

forceRefreshTaffyWindows :: TaffyIO ()
forceRefreshTaffyWindows =
  asks existingWindows >>= lift . flip MV.modifyMVar_ deleteWindows >>
       refreshTaffyWindows
    where deleteWindows windows =
            do
              mapM_ (Gtk.widgetDestroy . sel2) windows
              return []

asksContextVar :: (r -> MV.MVar b) -> ReaderT r IO b
asksContextVar getter = asks getter >>= lift . MV.readMVar

runX11 :: X11Property a -> TaffyIO a
runX11 action =
  asksContextVar x11ContextVar >>= lift . runReaderT action

runX11Def :: a -> X11Property a -> TaffyIO a
runX11Def def prop = runX11 $ postX11RequestSyncProp prop def

runX11Context :: MonadIO m => Context -> a -> X11Property a -> m a
runX11Context context def prop =
  liftIO $ runReaderT (runX11Def def prop) context

getState :: forall t. Typeable t => Taffy IO (Maybe t)
getState = do
  stateMap <- asksContextVar contextState
  let maybeValue = M.lookup (typeOf (undefined :: t)) stateMap
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
    let theType = typeOf (undefined :: t)
        currentValue = M.lookup theType contextStateMap
        insertAndReturn value =
          (M.insert theType (Value value) contextStateMap, value)
    in flip runReaderT ctx $  maybe
         (insertAndReturn  <$> getValue)
         (return . (contextStateMap,))
         (currentValue >>= fromValue)

-- | A version of "forkIO" in "TaffyIO".
taffyFork :: ReaderT r IO () -> ReaderT r IO ()
taffyFork = void . liftReader forkIO

startX11EventHandler :: Taffy IO ()
startX11EventHandler = taffyFork $ do
  c <- ask
  -- XXX: The event loop needs its own X11Context to separately handle
  -- communications from the X server. We deliberately avoid using the context
  -- from x11ContextVar here.
  lift $ withDefaultCtx $ eventLoop
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
