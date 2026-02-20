{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.Workspaces.EWMH
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Shared EWMH/X11 workspace-state provider using broadcast channels + state MVar.
module System.Taffybar.Information.Workspaces.EWMH
  ( EWMHWorkspaceProviderConfig (..),
    defaultEWMHWorkspaceProviderConfig,
    defaultEWMHWorkspaceState,
    getEWMHWorkspaceStateAndEventChansAndVar,
    getEWMHWorkspaceStateAndEventChansAndVarWith,
    getEWMHWorkspaceEventChan,
    getEWMHWorkspaceEventChanWith,
    getEWMHWorkspaceStateChanAndVar,
    getEWMHWorkspaceStateChanAndVarWith,
    getEWMHWorkspaceStateChan,
    getEWMHWorkspaceStateChanWith,
    getEWMHWorkspaceState,
    getEWMHWorkspaceStateWith,
  )
where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception.Enclosed (catchAny)
import Control.Monad (filterM, foldM, void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Control.RateLimit (RateLimit (PerInvocation), generateRateLimitedFunction)
import Data.Char (toLower)
import Data.List (isPrefixOf, (\\))
import qualified Data.MultiMap as MM
import qualified Data.Text as T
import Data.Time.Units (Microsecond, fromMicroseconds)
import System.Log.Logger (Priority (..), logM)
import System.Taffybar.Context
  ( TaffyIO,
    getStateDefault,
    runX11,
    subscribeToPropertyEvents,
    taffyFork,
  )
import System.Taffybar.Information.EWMHDesktopInfo
  ( WorkspaceId (WorkspaceId),
    allEWMHProperties,
    ewmhWMIcon,
    getActiveWindow,
    getVisibleWorkspaces,
    getWindowClass,
    getWindowMinimized,
    getWindowTitle,
    getWindows,
    getWorkspace,
    getWorkspaceNames,
    isWindowUrgent,
    parseWindowClasses,
  )
import System.Taffybar.Information.SafeX11 (safeGetGeometry)
import System.Taffybar.Information.Workspaces.Model
import System.Taffybar.Information.X11DesktopInfo (X11Property, X11Window, getDisplay)

data EWMHWorkspaceProviderConfig = EWMHWorkspaceProviderConfig
  { workspaceSnapshotGetter :: TaffyIO (Bool, [WorkspaceInfo]),
    workspaceUpdateEvents :: [String],
    workspaceUpdateRateLimitMicroseconds :: Integer
  }

defaultEWMHWorkspaceProviderConfig :: EWMHWorkspaceProviderConfig
defaultEWMHWorkspaceProviderConfig =
  EWMHWorkspaceProviderConfig
    { workspaceSnapshotGetter = buildEWMHWorkspaceSnapshot,
      workspaceUpdateEvents = allEWMHProperties \\ [ewmhWMIcon],
      workspaceUpdateRateLimitMicroseconds = 100000
    }

defaultEWMHWorkspaceState :: WorkspaceSnapshot
defaultEWMHWorkspaceState =
  WorkspaceSnapshot
    { snapshotBackend = WorkspaceBackendEWMH,
      snapshotRevision = 0,
      snapshotWindowDataComplete = False,
      snapshotWorkspaces = []
    }

newtype EWMHWorkspaceStateChanVar
  = EWMHWorkspaceStateChanVar
      (TChan WorkspaceSnapshot, TChan WorkspaceEventBatch, MVar WorkspaceSnapshot)

wLog :: (MonadIO m) => Priority -> String -> m ()
wLog level message =
  liftIO $ logM "System.Taffybar.Information.Workspaces.EWMH" level message

getEWMHWorkspaceStateAndEventChansAndVar ::
  TaffyIO (TChan WorkspaceSnapshot, TChan WorkspaceEventBatch, MVar WorkspaceSnapshot)
getEWMHWorkspaceStateAndEventChansAndVar =
  getEWMHWorkspaceStateAndEventChansAndVarWith defaultEWMHWorkspaceProviderConfig

getEWMHWorkspaceStateAndEventChansAndVarWith ::
  EWMHWorkspaceProviderConfig ->
  TaffyIO (TChan WorkspaceSnapshot, TChan WorkspaceEventBatch, MVar WorkspaceSnapshot)
getEWMHWorkspaceStateAndEventChansAndVarWith cfg = do
  EWMHWorkspaceStateChanVar chansAndVar <- getStateDefault $ buildEWMHWorkspaceStateChanVar cfg
  return chansAndVar

getEWMHWorkspaceEventChan :: TaffyIO (TChan WorkspaceEventBatch)
getEWMHWorkspaceEventChan =
  getEWMHWorkspaceEventChanWith defaultEWMHWorkspaceProviderConfig

getEWMHWorkspaceEventChanWith ::
  EWMHWorkspaceProviderConfig ->
  TaffyIO (TChan WorkspaceEventBatch)
getEWMHWorkspaceEventChanWith cfg = do
  (_, eventChan, _) <- getEWMHWorkspaceStateAndEventChansAndVarWith cfg
  return eventChan

getEWMHWorkspaceStateChanAndVar ::
  TaffyIO (TChan WorkspaceSnapshot, MVar WorkspaceSnapshot)
getEWMHWorkspaceStateChanAndVar =
  getEWMHWorkspaceStateChanAndVarWith defaultEWMHWorkspaceProviderConfig

getEWMHWorkspaceStateChanAndVarWith ::
  EWMHWorkspaceProviderConfig ->
  TaffyIO (TChan WorkspaceSnapshot, MVar WorkspaceSnapshot)
getEWMHWorkspaceStateChanAndVarWith cfg = do
  (stateChan, _, stateVar) <- getEWMHWorkspaceStateAndEventChansAndVarWith cfg
  return (stateChan, stateVar)

getEWMHWorkspaceStateChan :: TaffyIO (TChan WorkspaceSnapshot)
getEWMHWorkspaceStateChan =
  getEWMHWorkspaceStateChanWith defaultEWMHWorkspaceProviderConfig

getEWMHWorkspaceStateChanWith ::
  EWMHWorkspaceProviderConfig ->
  TaffyIO (TChan WorkspaceSnapshot)
getEWMHWorkspaceStateChanWith cfg =
  fst <$> getEWMHWorkspaceStateChanAndVarWith cfg

getEWMHWorkspaceState :: TaffyIO WorkspaceSnapshot
getEWMHWorkspaceState =
  getEWMHWorkspaceStateWith defaultEWMHWorkspaceProviderConfig

getEWMHWorkspaceStateWith ::
  EWMHWorkspaceProviderConfig ->
  TaffyIO WorkspaceSnapshot
getEWMHWorkspaceStateWith cfg = do
  (_, var) <- getEWMHWorkspaceStateChanAndVarWith cfg
  liftIO $ readMVar var

buildEWMHWorkspaceStateChanVar ::
  EWMHWorkspaceProviderConfig ->
  TaffyIO EWMHWorkspaceStateChanVar
buildEWMHWorkspaceStateChanVar cfg = do
  stateChan <- liftIO newBroadcastTChanIO
  eventChan <- liftIO newBroadcastTChanIO
  stateVar <- liftIO $ newMVar defaultEWMHWorkspaceState
  taffyFork $ ewmhWorkspaceStateLoop cfg stateChan eventChan stateVar
  return $ EWMHWorkspaceStateChanVar (stateChan, eventChan, stateVar)

ewmhWorkspaceStateLoop ::
  EWMHWorkspaceProviderConfig ->
  TChan WorkspaceSnapshot ->
  TChan WorkspaceEventBatch ->
  MVar WorkspaceSnapshot ->
  TaffyIO ()
ewmhWorkspaceStateLoop cfg stateChan eventChan stateVar = do
  refreshEWMHWorkspaceState cfg stateChan eventChan stateVar
  setupEWMHWorkspaceSubscription cfg stateChan eventChan stateVar
    `catchAny` \err ->
      wLog WARNING $ "Failed to subscribe to EWMH workspace events: " <> show err

setupEWMHWorkspaceSubscription ::
  EWMHWorkspaceProviderConfig ->
  TChan WorkspaceSnapshot ->
  TChan WorkspaceEventBatch ->
  MVar WorkspaceSnapshot ->
  TaffyIO ()
setupEWMHWorkspaceSubscription cfg stateChan eventChan stateVar = do
  ctx <- ask
  let rate = fromMicroseconds (workspaceUpdateRateLimitMicroseconds cfg) :: Microsecond
      combineRequests _ b = Just (b, const ((), ()))
      refreshForEvent _ =
        runReaderT
          (refreshEWMHWorkspaceState cfg stateChan eventChan stateVar)
          ctx
  rateLimitedRefresh <-
    liftIO $
      generateRateLimitedFunction
        (PerInvocation rate)
        refreshForEvent
        combineRequests
  _ <-
    subscribeToPropertyEvents
      (workspaceUpdateEvents cfg)
      (liftIO . void . rateLimitedRefresh)
  return ()

refreshEWMHWorkspaceState ::
  EWMHWorkspaceProviderConfig ->
  TChan WorkspaceSnapshot ->
  TChan WorkspaceEventBatch ->
  MVar WorkspaceSnapshot ->
  TaffyIO ()
refreshEWMHWorkspaceState cfg stateChan eventChan stateVar = do
  previous <- liftIO $ readMVar stateVar
  (complete, workspaces) <-
    workspaceSnapshotGetter cfg
      `catchAny` \err -> do
        wLog WARNING $ "EWMH workspace snapshot update failed: " <> show err
        return (False, snapshotWorkspaces previous)
  let nextRevision = snapshotRevision previous + 1
      next =
        WorkspaceSnapshot
          { snapshotBackend = WorkspaceBackendEWMH,
            snapshotRevision = nextRevision,
            snapshotWindowDataComplete = complete,
            snapshotWorkspaces = workspaces
          }
      eventBatch =
        WorkspaceEventBatch
          { eventBatchBackend = WorkspaceBackendEWMH,
            eventBatchRevision = nextRevision,
            eventBatchWindowDataComplete = complete,
            eventBatchEvents = diffWorkspaceSnapshots previous next
          }
  liftIO $ do
    _ <- swapMVar stateVar next
    atomically $ do
      writeTChan stateChan next
      writeTChan eventChan eventBatch

buildEWMHWorkspaceSnapshot :: TaffyIO (Bool, [WorkspaceInfo])
buildEWMHWorkspaceSnapshot = do
  workspaces <- runX11 collectEWMHWorkspaceSnapshot
  return (True, workspaces)

collectEWMHWorkspaceSnapshot :: X11Property [WorkspaceInfo]
collectEWMHWorkspaceSnapshot = do
  names <- getWorkspaceNames
  windows <- getWindows
  workspaceToWindows <- getWorkspaceToWindows windows
  urgentWindows <- filterM isWindowUrgent windows
  activeWindow <- getActiveWindow
  visibleWorkspaces <- getVisibleWorkspaces
  let activeWorkspace = case visibleWorkspaces of
        [] -> Nothing
        ws : _ -> Just ws
      nonFocusedVisibleWorkspaces = case visibleWorkspaces of
        [] -> []
        _ : rest -> rest
      toWorkspaceInfo (WorkspaceId idx, name) = do
        let workspaceKey = WorkspaceId idx
            wsWindowIds = MM.lookup workspaceKey workspaceToWindows
            wsViewState
              | Just workspaceKey == activeWorkspace = WorkspaceActive
              | workspaceKey `elem` nonFocusedVisibleWorkspaces = WorkspaceVisible
              | null wsWindowIds = WorkspaceEmpty
              | otherwise = WorkspaceHidden
        windowsInfo <- mapM (getWindowInfo activeWindow urgentWindows) wsWindowIds
        return
          WorkspaceInfo
            { workspaceIdentity =
                WorkspaceIdentity
                  { workspaceNumericId = Just idx,
                    workspaceName = T.pack name
                  },
              workspaceState = wsViewState,
              workspaceHasUrgentWindow = any windowUrgent windowsInfo,
              workspaceIsSpecial = isSpecialWorkspaceName name,
              workspaceWindows = windowsInfo
            }
  mapM toWorkspaceInfo names

isSpecialWorkspaceName :: String -> Bool
isSpecialWorkspaceName name =
  let lowered = map toLower name
   in lowered == "nsp" || "special:" `isPrefixOf` lowered

getWorkspaceToWindows ::
  [X11Window] ->
  X11Property (MM.MultiMap WorkspaceId X11Window)
getWorkspaceToWindows =
  foldM
    (\theMap window -> MM.insert <$> getWorkspace window <*> pure window <*> pure theMap)
    MM.empty

getWindowInfo ::
  Maybe X11Window ->
  [X11Window] ->
  X11Window ->
  X11Property WindowInfo
getWindowInfo activeWindow urgentWindows window = do
  wTitle <- getWindowTitle window
  wClass <- getWindowClass window
  wMinimized <- getWindowMinimized window
  wPosition <- getWindowPosition window
  return
    WindowInfo
      { windowIdentity = X11WindowIdentity (fromIntegral window),
        windowTitle = T.pack wTitle,
        windowClassHints = map T.pack $ parseWindowClasses wClass,
        windowPosition = wPosition,
        windowUrgent = window `elem` urgentWindows,
        windowActive = Just window == activeWindow,
        windowMinimized = wMinimized
      }

getWindowPosition :: X11Window -> X11Property (Maybe (Int, Int))
getWindowPosition window = do
  display <- getDisplay
  liftIO $
    ((\(_, x, y, _, _, _, _) -> Just (fromIntegral x, fromIntegral y)) <$> safeGetGeometry display window)
      `catchAny` const (return Nothing)
