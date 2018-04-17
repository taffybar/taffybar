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
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import qualified DBus.Client as DBus
import           Data.Data
import           Data.Int
import           Data.List
import qualified Data.Map as M
import           Data.Tuple.Select
import           Data.Tuple.Sequence
import           Data.Unique
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified GI.Gtk
import           Graphics.UI.GIGtkStrut
import           Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.General.StyleContext as Gtk
import qualified Graphics.UI.Gtk.Types as Gtk
import           System.Information.SafeX11
import           System.Information.X11DesktopInfo
import           System.Log.Logger
import           Text.Printf
import           Unsafe.Coerce

logIO = logM "System.Taffybar.Context"
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
  , errorMsg :: Maybe String
  }

defaultTaffybarConfig :: TaffybarConfig
defaultTaffybarConfig = TaffybarConfig
  { dbusClientParam = Nothing
  , startupHook = return ()
  , getBarConfigsParam = return []
  , errorMsg = Nothing
  }

data Context = Context
  { x11ContextVar :: MV.MVar X11Context
  , listeners :: MV.MVar SubscriptionList
  , contextState :: MV.MVar (M.Map TypeRep Value)
  , existingWindows :: MV.MVar [(BarConfig, Gtk.Window)]
  , dbusClient :: DBus.Client
  , getBarConfigs :: BarConfigGetter
  }

buildContext :: TaffybarConfig -> IO Context
buildContext TaffybarConfig
               { dbusClientParam = maybeDbus
               , getBarConfigsParam = barConfigGetter
               , startupHook = startup
               } = do
  logIO DEBUG "Building context"
  dbusC <- maybe DBus.connectSession return maybeDbus
  listenersVar <- MV.newMVar []
  state <- MV.newMVar M.empty
  x11Context <- getDefaultCtx >>= MV.newMVar
  windowsVar <- MV.newMVar []
  let context = Context
                { x11ContextVar = x11Context
                , listeners = listenersVar
                , contextState = state
                , dbusClient = dbusC
                , getBarConfigs = barConfigGetter
                , existingWindows = windowsVar
                }
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

gtk2hsToGIGtkWindow :: Gtk.Window -> IO GI.Gtk.Window
gtk2hsToGIGtkWindow window = do
  let wid = Gtk.toWidget window
  fPtr <- withForeignPtr (Gtk.unWidget wid) (flip GI.Gtk.newManagedPtr (return ()) . castPtr)
  return $! GI.Gtk.Window fPtr

buildBarWindow :: Context -> BarConfig -> IO Gtk.Window
buildBarWindow context barConfig = do
  logIO DEBUG $
      printf "Building bar window with StrutConfig: %s" $
      show $ strutConfig barConfig

  window <- Gtk.windowNew
  box <- Gtk.hBoxNew False $ fromIntegral $ widgetSpacing barConfig
  centerBox <- Gtk.hBoxNew False $ fromIntegral $ widgetSpacing barConfig
  Gtk.boxSetCenterWidget box centerBox

  -- XXX: This conversion could leak memory
  giWindow <- gtk2hsToGIGtkWindow window
  setupStrutWindow (strutConfig barConfig) giWindow
  Gtk.containerAdd window box

  styleContext <- Gtk.widgetGetStyleContext window
  Gtk.styleContextAddClass styleContext "Taffybar"

  let addWidgetWith widgetAdd buildWidget =
        do
          widget <- runReaderT buildWidget context
          -- XXX: This is a pretty bad way to do this
          let height =
                case strutHeight $ strutConfig barConfig of
                  ExactSize size -> fromIntegral size
                  _ -> 40
          Gtk.widgetSetSizeRequest widget (-1) height
          widgetAdd widget
      addToStart widget = Gtk.boxPackStart box widget Gtk.PackNatural 0
      addToEnd widget = Gtk.boxPackEnd box widget Gtk.PackNatural 0
      addToCenter widget = Gtk.boxPackStart centerBox widget Gtk.PackNatural 0

  logIO DEBUG "Building start widgets"
  mapM_ (addWidgetWith addToStart) (startWidgets barConfig)
  logIO DEBUG "Building center widgets"
  mapM_ (addWidgetWith addToCenter) (centerWidgets barConfig)
  logIO DEBUG "Building end widgets"
  mapM_ (addWidgetWith addToEnd) (endWidgets barConfig)

  widgetShow window
  widgetShow box
  widgetShow centerBox
  logIO DEBUG "Showing window"

  return window

refreshTaffyWindows :: TaffyIO ()
refreshTaffyWindows = liftReader Gtk.postGUIAsync $ do
  logT DEBUG "Refreshing windows"
  ctx <- ask
  windowsVar <- asks existingWindows

  let rebuildWindows currentWindows = flip runReaderT ctx $
        do
          barConfigs <- join $ asks getBarConfigs

          let currentConfigs = map sel1 currentWindows
              (_, newConfs) = partition (`elem` currentConfigs) barConfigs
              (remainingWindows, removedWindows) =
                partition ((`elem` barConfigs) . sel1) currentWindows
              setPropertiesFromPair (barConf, window) =
                gtk2hsToGIGtkWindow window >>=
                setupStrutWindow (strutConfig barConf)

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
            mapM (sequenceT . ((return :: a -> IO a) &&& buildBarWindow ctx)) newConfs

          return $ newWindowPairs ++ remainingWindows

  lift $ MV.modifyMVar_ windowsVar rebuildWindows
  logT DEBUG "Finished refreshing windows"
  return ()

asksContextVar :: (r -> MV.MVar b) -> ReaderT r IO b
asksContextVar getter = asks getter >>= lift . MV.readMVar

runX11 :: ReaderT X11Context IO b -> ReaderT Context IO b
runX11 action =
  asksContextVar x11ContextVar >>= lift . runReaderT action

runX11Def :: a -> X11Property a -> TaffyIO a
runX11Def def prop = runX11 $ postX11RequestSyncProp prop def

getState :: forall t. Typeable t => Taffy IO (Maybe t)
getState = do
  stateMap <- asksContextVar contextState
  let maybeValue = M.lookup (typeOf (undefined :: t)) stateMap
  return $ maybeValue >>= fromValue

getStateDefault :: Typeable t => Taffy IO t -> Taffy IO t
getStateDefault defaultGetter =
  getState >>= maybe (defaultGetter >>= putState) return

putState :: Typeable t => t -> Taffy IO t
putState v = do
  contextVar <- asks contextState
  lift $ MV.modifyMVar_ contextVar $ return . M.insert (typeOf v) (Value v)
  return v

liftReader ::
  Monad m => (m1 a -> m b) -> ReaderT r m1 a -> ReaderT r m b
liftReader modifier action =
  ask >>= lift . modifier . runReaderT action

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
    -- This type annotation probably has something to do with the warnings that
    -- occur without MonoLocalBinds, but it still seems to be necessary
    addListener :: SubscriptionList -> SubscriptionList
    addListener = ((identifier, listener):)
  lift $ MV.modifyMVar_ listenersVar (return . addListener)
  return identifier

subscribeToEvents :: [String] -> Listener -> Taffy IO Unique
subscribeToEvents eventNames listener = do
  eventAtoms <- mapM (runX11 . getAtom) eventNames
  let filteredListener event@PropertyEvent { ev_atom = atom } =
        when (atom `elem` eventAtoms) $ catchAny (listener event) (const $ return ())
      filteredListener _ = return ()
  subscribeToAll filteredListener

handleX11Event :: Event -> Taffy IO ()
handleX11Event event =
  asksContextVar listeners >>= mapM_ applyListener
  where applyListener :: (Unique, Listener) -> Taffy IO ()
        applyListener (_, listener) = taffyFork $ listener event
