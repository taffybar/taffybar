{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Data.Int
import           Data.List
import qualified Data.Map as M
import           Data.Tuple.Select
import           Data.Tuple.Sequence
import           Data.Unique
import qualified GI.Gdk
import qualified GI.GdkX11 as GdkX11
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

logT :: MonadTrans t => System.Log.Logger.Priority -> String -> t IO ()
logT p m = lift $ logIO p m

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
  , cssPath :: Maybe FilePath
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
  , cssPath = Nothing
  , errorMsg = Nothing
  }

data Context = Context
  { x11ContextVar :: MV.MVar X11Context
  , listeners :: MV.MVar SubscriptionList
  , contextState :: MV.MVar (M.Map TypeRep Value)
  , existingWindows :: MV.MVar [(BarConfig, Gtk.Window)]
  , sessionDBusClient :: DBus.Client
  , systemDBusClient :: DBus.Client
  , getBarConfigs :: BarConfigGetter
  , contextBarConfig :: Maybe BarConfig
  }

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
    logT DEBUG "Starting X11 Handler"
    startX11EventHandler
    logT DEBUG "Running startup hook"
    startup
    logT DEBUG "Queing build windows command"
    refreshTaffyWindows
  logIO DEBUG "Context build finished"
  return context

buildEmptyContext :: IO Context
buildEmptyContext = buildContext defaultTaffybarConfig

instance GdkX11.IsX11Window GI.Gdk.Window

buildBarWindow :: Context -> BarConfig -> IO Gtk.Window
buildBarWindow context barConfig = do
  let thisContext = context { contextBarConfig = Just barConfig }
  logIO DEBUG $
      printf "Building bar window with StrutConfig: %s" $
      show $ strutConfig barConfig

  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  box <- Gtk.boxNew Gtk.OrientationHorizontal $ fromIntegral $ widgetSpacing barConfig
  _ <- widgetSetClassGI box "taffy-box"
  centerBox <- Gtk.boxNew Gtk.OrientationHorizontal $ fromIntegral $ widgetSpacing barConfig
  Gtk.boxSetCenterWidget box (Just centerBox)

  setupStrutWindow (strutConfig barConfig) window
  Gtk.containerAdd window box

  _ <- widgetSetClassGI window "taffy-window"

  let addWidgetWith widgetAdd buildWidget =
        runReaderT buildWidget thisContext >>= widgetAdd
      addToStart widget = Gtk.boxPackStart box widget False False 0
      addToEnd widget = Gtk.boxPackEnd box widget False False 0
      addToCenter widget = Gtk.boxPackStart centerBox widget False False 0

  logIO DEBUG "Building start widgets"
  mapM_ (addWidgetWith addToStart) (startWidgets barConfig)
  logIO DEBUG "Building center widgets"
  mapM_ (addWidgetWith addToCenter) (centerWidgets barConfig)
  logIO DEBUG "Building end widgets"
  mapM_ (addWidgetWith addToEnd) (endWidgets barConfig)

  makeWindowTransparent window

  logIO DEBUG "Showing window"
  Gtk.widgetShow window
  Gtk.widgetShow box
  Gtk.widgetShow centerBox

  runX11Context context () $ void $ runMaybeT $ do
    gdkWindow <- MaybeT $ Gtk.widgetGetWindow window
    xid <- GdkX11.x11WindowGetXid gdkWindow
    lift $ doLowerWindow (fromIntegral xid)

  return window

refreshTaffyWindows :: TaffyIO ()
refreshTaffyWindows = liftReader postGUIASync $ do
  logT DEBUG "Refreshing windows"
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
  logT DEBUG "Finished refreshing windows"
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

taffyFork :: ReaderT r IO () -> ReaderT r IO ()
taffyFork = void . liftReader forkIO

startX11EventHandler :: Taffy IO ()
startX11EventHandler = taffyFork $ do
  c <- ask
  -- The event loop needs its own X11Context to separately handle communications
  -- from the X server.
  lift $ withDefaultCtx $ eventLoop
         (\e -> runReaderT (handleX11Event e) c)

unsubscribe :: Unique -> Taffy IO ()
unsubscribe identifier = do
  listenersVar <- asks listeners
  lift $ MV.modifyMVar_ listenersVar $ return . filter ((== identifier) . fst)

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

subscribeToEvents :: [String] -> Listener -> Taffy IO Unique
subscribeToEvents eventNames listener = do
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
