-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.Workspaces.Model
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Backend-agnostic workspace/window model shared by workspace-related widgets.
module System.Taffybar.Information.Workspaces.Model
  ( WorkspaceBackend (..),
    WorkspaceSnapshot (..),
    WorkspaceEventBatch (..),
    WorkspaceEvent (..),
    WindowEvent (..),
    diffWorkspaceSnapshots,
    WorkspaceViewState (..),
    WorkspaceIdentity (..),
    WorkspaceInfo (..),
    WindowIdentity (..),
    WindowInfo (..),
  )
where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Word (Word64)

data WorkspaceBackend
  = WorkspaceBackendEWMH
  | WorkspaceBackendHyprland
  deriving (Eq, Show)

-- | Snapshot of workspace state produced by a backend provider.
data WorkspaceSnapshot = WorkspaceSnapshot
  { snapshotBackend :: WorkspaceBackend,
    snapshotRevision :: Word64,
    -- | Whether window-level data was complete in this snapshot.
    snapshotWindowDataComplete :: Bool,
    snapshotWorkspaces :: [WorkspaceInfo]
  }
  deriving (Eq, Show)

-- | Delta events corresponding to a snapshot revision.
--
-- Consumers that want incremental updates can subscribe to these events,
-- while snapshot consumers keep using 'WorkspaceSnapshot'.
data WorkspaceEventBatch = WorkspaceEventBatch
  { eventBatchBackend :: WorkspaceBackend,
    eventBatchRevision :: Word64,
    eventBatchWindowDataComplete :: Bool,
    eventBatchEvents :: [WorkspaceEvent]
  }
  deriving (Eq, Show)

data WorkspaceEvent
  = WorkspaceOrderChanged [WorkspaceIdentity]
  | WorkspaceAdded WorkspaceInfo
  | WorkspaceRemoved WorkspaceIdentity
  | WorkspaceChanged WorkspaceInfo
  | WorkspaceWindowsChanged WorkspaceIdentity [WindowEvent]
  deriving (Eq, Show)

data WindowEvent
  = WindowAdded WindowInfo
  | WindowRemoved WindowIdentity
  | WindowChanged WindowInfo
  deriving (Eq, Show)

-- | Visibility/occupancy state that controls workspace presentation.
data WorkspaceViewState
  = WorkspaceActive
  | WorkspaceVisible
  | WorkspaceHidden
  | WorkspaceEmpty
  deriving (Eq, Show)

-- | Backend-neutral workspace identity.
data WorkspaceIdentity = WorkspaceIdentity
  { workspaceNumericId :: Maybe Int,
    workspaceName :: Text
  }
  deriving (Eq, Ord, Show)

-- | Backend-neutral workspace information.
data WorkspaceInfo = WorkspaceInfo
  { workspaceIdentity :: WorkspaceIdentity,
    workspaceState :: WorkspaceViewState,
    workspaceHasUrgentWindow :: Bool,
    workspaceIsSpecial :: Bool,
    workspaceWindows :: [WindowInfo]
  }
  deriving (Eq, Show)

data WindowIdentity
  = X11WindowIdentity Word64
  | HyprlandWindowIdentity Text
  deriving (Eq, Ord, Show)

-- | Backend-neutral window information.
data WindowInfo = WindowInfo
  { windowIdentity :: WindowIdentity,
    windowTitle :: Text,
    -- | Ordered class/app-id hints used for icon lookup.
    windowClassHints :: [Text],
    -- | Top-left position in global coordinates when available.
    windowPosition :: Maybe (Int, Int),
    windowUrgent :: Bool,
    windowActive :: Bool,
    windowMinimized :: Bool
  }
  deriving (Eq, Show)

-- | Produce incremental events that describe the change from an old snapshot to
-- a new one.
diffWorkspaceSnapshots :: WorkspaceSnapshot -> WorkspaceSnapshot -> [WorkspaceEvent]
diffWorkspaceSnapshots previous next =
  orderEvent ++ removedEvents ++ addedEvents ++ changedEvents
  where
    oldWorkspaces = snapshotWorkspaces previous
    newWorkspaces = snapshotWorkspaces next
    oldOrder = map workspaceIdentity oldWorkspaces
    newOrder = map workspaceIdentity newWorkspaces
    orderEvent =
      [WorkspaceOrderChanged newOrder | oldOrder /= newOrder]
    oldMap = M.fromList [(workspaceIdentity ws, ws) | ws <- oldWorkspaces]
    newMap = M.fromList [(workspaceIdentity ws, ws) | ws <- newWorkspaces]
    removedEvents =
      [WorkspaceRemoved wsId | wsId <- M.keys oldMap, M.notMember wsId newMap]
    addedEvents =
      [WorkspaceAdded ws | (wsId, ws) <- M.toList newMap, M.notMember wsId oldMap]
    changedEvents =
      concatMap mkChangedEvents sharedIds
    sharedIds = M.keys $ M.intersection oldMap newMap
    mkChangedEvents wsId =
      case (M.lookup wsId oldMap, M.lookup wsId newMap) of
        (Just oldWs, Just newWs) ->
          let oldWithoutWindows = oldWs {workspaceWindows = []}
              newWithoutWindows = newWs {workspaceWindows = []}
              metadataEvents =
                [WorkspaceChanged newWs | oldWithoutWindows /= newWithoutWindows]
              windowEvents = diffWorkspaceWindows oldWs newWs
              windowsChangedEvents =
                [ WorkspaceWindowsChanged wsId windowEvents
                | not (null windowEvents)
                ]
           in metadataEvents ++ windowsChangedEvents
        _ -> []

diffWorkspaceWindows :: WorkspaceInfo -> WorkspaceInfo -> [WindowEvent]
diffWorkspaceWindows oldWs newWs =
  removedEvents ++ addedEvents ++ changedEvents
  where
    oldWindows = workspaceWindows oldWs
    newWindows = workspaceWindows newWs
    oldMap = M.fromList [(windowIdentity w, w) | w <- oldWindows]
    newMap = M.fromList [(windowIdentity w, w) | w <- newWindows]
    removedEvents =
      [WindowRemoved wId | wId <- M.keys oldMap, M.notMember wId newMap]
    addedEvents =
      [WindowAdded w | (wId, w) <- M.toList newMap, M.notMember wId oldMap]
    changedEvents =
      [ WindowChanged newW
      | wId <- M.keys $ M.intersection oldMap newMap,
        Just oldW <- [M.lookup wId oldMap],
        Just newW <- [M.lookup wId newMap],
        oldW /= newW
      ]
