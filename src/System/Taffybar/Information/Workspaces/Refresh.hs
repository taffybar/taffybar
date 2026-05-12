-- | Shared workspace refresh requests.
module System.Taffybar.Information.Workspaces.Refresh
  ( getWorkspacesRefreshRequestChan,
    requestWorkspacesRefresh,
    workspaceRefreshRequestLoop,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar_)
import Control.Concurrent.STM.TChan
  ( TChan,
    dupTChan,
    newBroadcastTChanIO,
    readTChan,
    writeTChan,
  )
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Information.Workspaces.Model

newtype WorkspacesRefreshRequestChan
  = WorkspacesRefreshRequestChan (TChan WorkspacesRefreshTarget)

getWorkspacesRefreshRequestChan :: TaffyIO (TChan WorkspacesRefreshTarget)
getWorkspacesRefreshRequestChan = do
  WorkspacesRefreshRequestChan chan <-
    getStateDefault $
      WorkspacesRefreshRequestChan <$> liftIO newBroadcastTChanIO
  return chan

requestWorkspacesRefresh :: WorkspacesRefreshTarget -> TaffyIO ()
requestWorkspacesRefresh target = do
  chan <- getWorkspacesRefreshRequestChan
  liftIO $ atomically $ writeTChan chan target

workspaceRefreshRequestLoop ::
  TChan WorkspaceSnapshot ->
  TChan WorkspaceEventBatch ->
  MVar WorkspaceSnapshot ->
  TaffyIO ()
workspaceRefreshRequestLoop stateChan eventChan stateVar = do
  requestChan <- getWorkspacesRefreshRequestChan
  requests <- liftIO $ atomically $ dupTChan requestChan
  forever $ do
    target <- liftIO $ atomically $ readTChan requests
    liftIO $ publishRefreshTarget stateChan eventChan stateVar target

publishRefreshTarget ::
  TChan WorkspaceSnapshot ->
  TChan WorkspaceEventBatch ->
  MVar WorkspaceSnapshot ->
  WorkspacesRefreshTarget ->
  IO ()
publishRefreshTarget stateChan eventChan stateVar target =
  modifyMVar_ stateVar $ \previous -> do
    let refreshed = bumpWorkspaceRefreshTarget target previous
        changed = snapshotWorkspaces refreshed /= snapshotWorkspaces previous
        next =
          refreshed
            { snapshotRevision = snapshotRevision previous + 1
            }
        eventBatch =
          WorkspaceEventBatch
            { eventBatchBackend = snapshotBackend next,
              eventBatchRevision = snapshotRevision next,
              eventBatchWindowDataComplete = snapshotWindowDataComplete next,
              eventBatchEvents = diffWorkspaceSnapshots previous next
            }
    when changed $
      atomically $ do
        writeTChan stateChan next
        writeTChan eventChan eventBatch
    return $ if changed then next else previous
