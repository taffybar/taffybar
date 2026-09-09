-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.DiskUsage
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Disk usage information using the @statvfs(2)@ system call (via the
-- @disk-free-space@ package).
--
-- The shared-channel API ('getDiskUsageInfoChan', 'getDiskUsageInfoState')
-- shares polling threads by canonical path and interval, so multiple bar
-- instances can reuse a poller while monitoring different filesystems.
module System.Taffybar.Information.DiskUsage
  ( DiskUsageInfo (..),
    forceDiskUsageRefresh,
    getDiskUsageInfo,
    getDiskUsageInfoChan,
    getDiskUsageInfoState,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception.Enclosed (catchAny)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically, orElse)
import Control.Monad.Trans.Reader (ask, runReaderT)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import System.Directory (canonicalizePath)
import System.DiskSpace (diskAvail, diskFree, diskTotal, getDiskUsage)
import System.Log.Logger (Priority (..))
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Information.Wakeup (getWakeupChannelNanoseconds, intervalSecondsToNanoseconds)
import System.Taffybar.Util (logPrintF)

-- | Disk usage statistics for a single filesystem.
data DiskUsageInfo = DiskUsageInfo
  { -- | Total space in bytes.
    diskInfoTotal :: !Integer,
    -- | Free space in bytes (includes reserved blocks).
    diskInfoFree :: !Integer,
    -- | Space available to unprivileged users, in bytes.
    diskInfoAvailable :: !Integer,
    -- | Used space in bytes (@total - free@).
    diskInfoUsed :: !Integer,
    -- | Percentage of total space that is used.
    diskInfoUsedPercent :: !Double,
    -- | Percentage of total space available to unprivileged users.
    diskInfoFreePercent :: !Double
  }
  deriving (Show, Eq)

-- | Query disk usage for the filesystem containing @path@ via @statvfs(2)@.
getDiskUsageInfo :: FilePath -> IO DiskUsageInfo
getDiskUsageInfo path = do
  du <- getDiskUsage path
  let total = diskTotal du
      free = diskFree du
      avail = diskAvail du
      used = total - free
      usedPct =
        if total > 0
          then fromIntegral used * 100.0 / fromIntegral total
          else 0
      freePct =
        if total > 0
          then fromIntegral avail * 100.0 / fromIntegral total
          else 0
  return
    DiskUsageInfo
      { diskInfoTotal = total,
        diskInfoFree = free,
        diskInfoAvailable = avail,
        diskInfoUsed = used,
        diskInfoUsedPercent = usedPct,
        diskInfoFreePercent = freePct
      }

-- --------------------------------------------------------------------------
-- Shared polling channel

newtype DiskUsageChanVar
  = DiskUsageChanVar
      ( TChan DiskUsageInfo,
        MVar DiskUsageInfo,
        TChan ()
      )

newtype DiskUsageSources
  = DiskUsageSources (MVar (Map.Map (FilePath, Word64) DiskUsageChanVar))

-- | Get a broadcast channel that is updated by a shared polling thread.
-- Calls with the same path and interval reuse the same poller.
getDiskUsageInfoChan :: Double -> FilePath -> TaffyIO (TChan DiskUsageInfo)
getDiskUsageInfoChan interval path = do
  DiskUsageChanVar (chan, _, _) <- setupDiskUsageChanVar interval path
  pure chan

-- | Read the latest cached 'DiskUsageInfo' from the shared poller.
getDiskUsageInfoState :: Double -> FilePath -> TaffyIO DiskUsageInfo
getDiskUsageInfoState interval path = do
  DiskUsageChanVar (_, var, _) <- setupDiskUsageChanVar interval path
  liftIO $ readMVar var

-- | Request an immediate disk usage refresh.
forceDiskUsageRefresh :: Double -> FilePath -> TaffyIO ()
forceDiskUsageRefresh interval path = do
  DiskUsageChanVar (_, _, refreshChan) <- setupDiskUsageChanVar interval path
  liftIO $ atomically $ writeTChan refreshChan ()

setupDiskUsageChanVar :: Double -> FilePath -> TaffyIO DiskUsageChanVar
setupDiskUsageChanVar interval path = do
  intervalNs <- either fail pure $ intervalSecondsToNanoseconds interval
  canonicalPath <- liftIO $ canonicalizePath path
  DiskUsageSources sources <- getStateDefault $ liftIO $ DiskUsageSources <$> newMVar Map.empty
  context <- ask
  liftIO $ modifyMVar sources $ \current -> do
    let key = (canonicalPath, intervalNs)
    case Map.lookup key current of
      Just source -> pure (current, source)
      Nothing -> do
        source <- runReaderT (buildDiskUsageSource intervalNs canonicalPath) context
        pure (Map.insert key source current, source)

buildDiskUsageSource :: Word64 -> FilePath -> TaffyIO DiskUsageChanVar
buildDiskUsageSource intervalNs path = do
  chan <- liftIO newBroadcastTChanIO
  refreshChan <- liftIO newTChanIO
  info <- liftIO $ getDiskUsageInfo path
  var <- liftIO $ newMVar info
  wakeupChan <- getWakeupChannelNanoseconds intervalNs
  ourWakeupChan <- liftIO $ atomically $ dupTChan wakeupChan
  void $
    liftIO $
      forkIO $
        forever $ do
          atomically $
            void (readTChan refreshChan)
              `orElse` void (readTChan ourWakeupChan)
          refreshDiskUsageState path chan var
  pure $ DiskUsageChanVar (chan, var, refreshChan)

refreshDiskUsageState ::
  FilePath ->
  TChan DiskUsageInfo ->
  MVar DiskUsageInfo ->
  IO ()
refreshDiskUsageState path chan var =
  catchAny
    ( do
        newInfo <- getDiskUsageInfo path
        void $ swapMVar var newInfo
        atomically $ writeTChan chan newInfo
    )
    (logPrintF logName WARNING "DiskUsage poll failed: %s")

logName :: String
logName = "System.Taffybar.Information.DiskUsage"
