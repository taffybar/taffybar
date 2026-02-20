{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.Workspaces.Hyprland
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Shared Hyprland workspace-state provider using a broadcast channel + state MVar.
module System.Taffybar.Information.Workspaces.Hyprland
  ( HyprlandWorkspaceProviderConfig (..),
    defaultHyprlandWorkspaceProviderConfig,
    defaultHyprlandWorkspaceState,
    isRelevantHyprlandWorkspaceEvent,
    getHyprlandWorkspaceStateAndEventChansAndVar,
    getHyprlandWorkspaceStateAndEventChansAndVarWith,
    getHyprlandWorkspaceEventChan,
    getHyprlandWorkspaceEventChanWith,
    getHyprlandWorkspaceStateChanAndVar,
    getHyprlandWorkspaceStateChanAndVarWith,
    getHyprlandWorkspaceStateChan,
    getHyprlandWorkspaceStateChanWith,
    getHyprlandWorkspaceState,
    getHyprlandWorkspaceStateWith,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception.Enclosed (catchAny)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.STM (atomically)
import Data.Either (isRight)
import qualified Data.Foldable as F
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Text as T
import System.Log.Logger (Priority (..), logM)
import System.Taffybar.Context (TaffyIO, getStateDefault, taffyFork)
import System.Taffybar.Hyprland (getHyprlandClient, getHyprlandEventChan)
import qualified System.Taffybar.Information.Hyprland as Hypr
import qualified System.Taffybar.Information.Hyprland.API as HyprAPI
import qualified System.Taffybar.Information.Hyprland.Types as HyprTypes
import System.Taffybar.Information.Workspaces.Model

data HyprlandWorkspaceProviderConfig = HyprlandWorkspaceProviderConfig
  { workspaceSnapshotGetter :: TaffyIO (Bool, [WorkspaceInfo]),
    workspaceEventFilter :: T.Text -> Bool
  }

defaultHyprlandWorkspaceProviderConfig :: HyprlandWorkspaceProviderConfig
defaultHyprlandWorkspaceProviderConfig =
  HyprlandWorkspaceProviderConfig
    { workspaceSnapshotGetter = buildHyprlandWorkspaceSnapshot,
      workspaceEventFilter = isRelevantHyprlandWorkspaceEvent
    }

defaultHyprlandWorkspaceState :: WorkspaceSnapshot
defaultHyprlandWorkspaceState =
  WorkspaceSnapshot
    { snapshotBackend = WorkspaceBackendHyprland,
      snapshotRevision = 0,
      snapshotWindowDataComplete = False,
      snapshotWorkspaces = []
    }

newtype HyprlandWorkspaceStateChanVar
  = HyprlandWorkspaceStateChanVar
      (TChan WorkspaceSnapshot, TChan WorkspaceEventBatch, MVar WorkspaceSnapshot)

wLog :: (MonadIO m) => Priority -> String -> m ()
wLog level msg = liftIO $ logM "System.Taffybar.Information.Workspaces.Hyprland" level msg

-- | Parse Hyprland event-socket lines and determine whether workspace state
-- should be refreshed.
isRelevantHyprlandWorkspaceEvent :: T.Text -> Bool
isRelevantHyprlandWorkspaceEvent line =
  let eventName = T.takeWhile (/= '>') line
   in eventName
        `elem` [ "workspace",
                 "workspacev2",
                 "focusedmon",
                 "activewindow",
                 "activewindowv2",
                 "openwindow",
                 "closewindow",
                 "movewindow",
                 "movewindowv2",
                 "moveworkspace",
                 "renameworkspace",
                 "createworkspace",
                 "destroyworkspace",
                 "monitoradded",
                 "monitorremoved",
                 "taffybar-hyprland-connected"
               ]

getHyprlandWorkspaceStateAndEventChansAndVar ::
  TaffyIO (TChan WorkspaceSnapshot, TChan WorkspaceEventBatch, MVar WorkspaceSnapshot)
getHyprlandWorkspaceStateAndEventChansAndVar =
  getHyprlandWorkspaceStateAndEventChansAndVarWith defaultHyprlandWorkspaceProviderConfig

getHyprlandWorkspaceStateAndEventChansAndVarWith ::
  HyprlandWorkspaceProviderConfig ->
  TaffyIO (TChan WorkspaceSnapshot, TChan WorkspaceEventBatch, MVar WorkspaceSnapshot)
getHyprlandWorkspaceStateAndEventChansAndVarWith cfg = do
  HyprlandWorkspaceStateChanVar chansAndVar <- getStateDefault $ buildHyprlandWorkspaceStateChanVar cfg
  return chansAndVar

getHyprlandWorkspaceEventChan :: TaffyIO (TChan WorkspaceEventBatch)
getHyprlandWorkspaceEventChan =
  getHyprlandWorkspaceEventChanWith defaultHyprlandWorkspaceProviderConfig

getHyprlandWorkspaceEventChanWith ::
  HyprlandWorkspaceProviderConfig ->
  TaffyIO (TChan WorkspaceEventBatch)
getHyprlandWorkspaceEventChanWith cfg = do
  (_, eventChan, _) <- getHyprlandWorkspaceStateAndEventChansAndVarWith cfg
  return eventChan

getHyprlandWorkspaceStateChanAndVar ::
  TaffyIO (TChan WorkspaceSnapshot, MVar WorkspaceSnapshot)
getHyprlandWorkspaceStateChanAndVar =
  getHyprlandWorkspaceStateChanAndVarWith defaultHyprlandWorkspaceProviderConfig

-- | Obtain the shared Hyprland workspace state channel and state MVar.
--
-- Note: like other 'getStateDefault' based providers, the first caller wins:
-- if this provider has already been initialized, later calls with a different
-- config return the existing provider state.
getHyprlandWorkspaceStateChanAndVarWith ::
  HyprlandWorkspaceProviderConfig ->
  TaffyIO (TChan WorkspaceSnapshot, MVar WorkspaceSnapshot)
getHyprlandWorkspaceStateChanAndVarWith cfg = do
  (stateChan, _, stateVar) <- getHyprlandWorkspaceStateAndEventChansAndVarWith cfg
  return (stateChan, stateVar)

getHyprlandWorkspaceStateChan :: TaffyIO (TChan WorkspaceSnapshot)
getHyprlandWorkspaceStateChan =
  getHyprlandWorkspaceStateChanWith defaultHyprlandWorkspaceProviderConfig

getHyprlandWorkspaceStateChanWith ::
  HyprlandWorkspaceProviderConfig ->
  TaffyIO (TChan WorkspaceSnapshot)
getHyprlandWorkspaceStateChanWith cfg =
  fst <$> getHyprlandWorkspaceStateChanAndVarWith cfg

getHyprlandWorkspaceState :: TaffyIO WorkspaceSnapshot
getHyprlandWorkspaceState =
  getHyprlandWorkspaceStateWith defaultHyprlandWorkspaceProviderConfig

getHyprlandWorkspaceStateWith ::
  HyprlandWorkspaceProviderConfig ->
  TaffyIO WorkspaceSnapshot
getHyprlandWorkspaceStateWith cfg = do
  (_, var) <- getHyprlandWorkspaceStateChanAndVarWith cfg
  liftIO $ readMVar var

buildHyprlandWorkspaceStateChanVar ::
  HyprlandWorkspaceProviderConfig ->
  TaffyIO HyprlandWorkspaceStateChanVar
buildHyprlandWorkspaceStateChanVar cfg = do
  stateChan <- liftIO newBroadcastTChanIO
  eventChan <- liftIO newBroadcastTChanIO
  var <- liftIO $ newMVar defaultHyprlandWorkspaceState
  taffyFork $ hyprlandWorkspaceStateLoop cfg stateChan eventChan var
  return $ HyprlandWorkspaceStateChanVar (stateChan, eventChan, var)

hyprlandWorkspaceStateLoop ::
  HyprlandWorkspaceProviderConfig ->
  TChan WorkspaceSnapshot ->
  TChan WorkspaceEventBatch ->
  MVar WorkspaceSnapshot ->
  TaffyIO ()
hyprlandWorkspaceStateLoop cfg stateChan workspaceEventChan var = do
  refreshHyprlandWorkspaceState cfg stateChan workspaceEventChan var
  hyprlandEventChan <- getHyprlandEventChan
  events <- liftIO $ Hypr.subscribeHyprlandEvents hyprlandEventChan
  forever $ do
    line <- liftIO $ atomically $ readTChan events
    when (workspaceEventFilter cfg line) $
      refreshHyprlandWorkspaceState cfg stateChan workspaceEventChan var

refreshHyprlandWorkspaceState ::
  HyprlandWorkspaceProviderConfig ->
  TChan WorkspaceSnapshot ->
  TChan WorkspaceEventBatch ->
  MVar WorkspaceSnapshot ->
  TaffyIO ()
refreshHyprlandWorkspaceState cfg stateChan workspaceEventChan var = do
  previous <- liftIO $ readMVar var
  (complete, workspaces) <-
    workspaceSnapshotGetter cfg
      `catchAny` \err -> do
        wLog WARNING $ "Hyprland workspace snapshot update failed: " <> show err
        return (False, snapshotWorkspaces previous)
  let next =
        WorkspaceSnapshot
          { snapshotBackend = WorkspaceBackendHyprland,
            snapshotRevision = snapshotRevision previous + 1,
            snapshotWindowDataComplete = complete,
            snapshotWorkspaces = workspaces
          }
      eventBatch =
        WorkspaceEventBatch
          { eventBatchBackend = snapshotBackend next,
            eventBatchRevision = snapshotRevision next,
            eventBatchWindowDataComplete = snapshotWindowDataComplete next,
            eventBatchEvents = diffWorkspaceSnapshots previous next
          }
  liftIO $ do
    _ <- swapMVar var next
    atomically $ do
      writeTChan stateChan next
      writeTChan workspaceEventChan eventBatch

buildHyprlandWorkspaceSnapshot :: TaffyIO (Bool, [WorkspaceInfo])
buildHyprlandWorkspaceSnapshot = do
  client <- getHyprlandClient
  workspacesResult <- liftIO $ HyprAPI.getHyprlandWorkspaces client
  clientsResult <- liftIO $ HyprAPI.getHyprlandClients client
  monitorsResult <- liftIO $ HyprAPI.getHyprlandMonitors client
  activeWorkspaceResult <- liftIO $ HyprAPI.getHyprlandActiveWorkspace client
  activeWindowResult <- liftIO $ HyprAPI.getHyprlandActiveWindow client

  workspaces <- case workspacesResult of
    Left err -> wLog WARNING ("hyprctl workspaces failed: " <> show err) >> return []
    Right ws -> return ws

  clients <- case clientsResult of
    Left err -> wLog WARNING ("hyprctl clients failed: " <> show err) >> return []
    Right cs -> return cs

  monitors <- case monitorsResult of
    Left err -> wLog WARNING ("hyprctl monitors failed: " <> show err) >> return []
    Right ms -> return ms

  activeWorkspaceId <- case activeWorkspaceResult of
    Left err -> wLog WARNING ("hyprctl activeworkspace failed: " <> show err) >> return Nothing
    Right ws -> return $ HyprTypes.hyprActiveWorkspaceId ws

  activeWindowAddress <- case activeWindowResult of
    Left err -> wLog WARNING ("hyprctl activewindow failed: " <> show err) >> return Nothing
    Right win ->
      let address = HyprTypes.hyprActiveWindowAddress win
       in return $ if T.null address then Nothing else Just address

  let windowsByWorkspace = collectWorkspaceWindows activeWindowAddress clients
      sortedWorkspaces = sortOn HyprTypes.hyprWorkspaceId workspaces
      visibleWorkspaceIds =
        [ HyprTypes.hyprWorkspaceRefId wsRef
        | monitor <- monitors,
          Just wsRef <- [HyprTypes.hyprMonitorActiveWorkspace monitor]
        ]
      focusedWorkspaceId =
        listToMaybe
          [ HyprTypes.hyprWorkspaceRefId wsRef
          | monitor <- monitors,
            HyprTypes.hyprMonitorFocused monitor,
            Just wsRef <- [HyprTypes.hyprMonitorActiveWorkspace monitor]
          ]
      activeWorkspaceId' = focusedWorkspaceId <|> activeWorkspaceId
      clientsOk = isRight clientsResult
      toWorkspace wsInfo =
        let wsId = HyprTypes.hyprWorkspaceId wsInfo
            wsName = HyprTypes.hyprWorkspaceName wsInfo
            wsWindows = M.findWithDefault [] wsId windowsByWorkspace
            hasWindows =
              not (null wsWindows)
                || fromMaybe 0 (HyprTypes.hyprWorkspaceWindows wsInfo) > 0
            wsState
              | Just wsId == activeWorkspaceId' = WorkspaceActive
              | wsId `elem` visibleWorkspaceIds = WorkspaceVisible
              | not hasWindows && clientsOk = WorkspaceEmpty
              | otherwise = WorkspaceHidden
         in WorkspaceInfo
              { workspaceIdentity =
                  WorkspaceIdentity
                    { workspaceNumericId = Just wsId,
                      workspaceName = wsName
                    },
                workspaceState = wsState,
                workspaceHasUrgentWindow = any windowUrgent wsWindows,
                workspaceIsSpecial = isSpecialWorkspace wsId wsName,
                workspaceWindows = wsWindows
              }
  return (clientsOk, map toWorkspace sortedWorkspaces)

isSpecialWorkspace :: Int -> T.Text -> Bool
isSpecialWorkspace wsId wsName =
  let lowered = T.toLower wsName
   in wsId < 0 || T.isPrefixOf "special" lowered

collectWorkspaceWindows ::
  Maybe T.Text ->
  [HyprTypes.HyprlandClientInfo] ->
  M.Map Int [WindowInfo]
collectWorkspaceWindows activeWindowAddress =
  F.foldl' (addWindow activeWindowAddress) M.empty
  where
    addWindow activeAddr windowsMap client =
      let wsId = HyprTypes.hyprWorkspaceRefId $ HyprTypes.hyprClientWorkspace client
          windowData = windowFromClient activeAddr client
       in M.insertWith (++) wsId [windowData] windowsMap

windowFromClient :: Maybe T.Text -> HyprTypes.HyprlandClientInfo -> WindowInfo
windowFromClient activeAddr client =
  let rawTitle = HyprTypes.hyprClientTitle client
      title =
        if T.null rawTitle
          then fromMaybe "" (HyprTypes.hyprClientInitialTitle client)
          else rawTitle
      active =
        HyprTypes.hyprClientFocused client
          || Just (HyprTypes.hyprClientAddress client) == activeAddr
      minimized =
        HyprTypes.hyprClientHidden client
          || not (HyprTypes.hyprClientMapped client)
   in WindowInfo
        { windowIdentity = HyprlandWindowIdentity (HyprTypes.hyprClientAddress client),
          windowTitle = title,
          windowClassHints =
            catMaybes
              [ HyprTypes.hyprClientClass client,
                HyprTypes.hyprClientInitialClass client
              ],
          windowPosition = HyprTypes.hyprClientAt client,
          windowUrgent = HyprTypes.hyprClientUrgent client,
          windowActive = active,
          windowMinimized = minimized
        }
