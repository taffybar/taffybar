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
-- uses a single polling thread per process (via 'getStateDefault') so that
-- multiple bar instances do not each spawn their own poller.
--
-- Because the channel is keyed by the 'DiskUsageChanVar' newtype, only one
-- monitored path is supported through the shared API.  If you need to
-- monitor several mount points independently, call 'getDiskUsageInfo'
-- directly with 'pollingLabelNew'.
-----------------------------------------------------------------------------

module System.Taffybar.Information.DiskUsage
  ( DiskUsageInfo(..)
  , getDiskUsageInfo
  , getDiskUsageInfoChan
  , getDiskUsageInfoState
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception.Enclosed (catchAny)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import System.DiskSpace (getDiskUsage, diskTotal, diskFree, diskAvail)
import System.Log.Logger (Priority(..))
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Util (logPrintF)

-- | Disk usage statistics for a single filesystem.
data DiskUsageInfo = DiskUsageInfo
  { diskInfoTotal        :: !Integer
  -- ^ Total space in bytes.
  , diskInfoFree         :: !Integer
  -- ^ Free space in bytes (includes reserved blocks).
  , diskInfoAvailable    :: !Integer
  -- ^ Space available to unprivileged users, in bytes.
  , diskInfoUsed         :: !Integer
  -- ^ Used space in bytes (@total - free@).
  , diskInfoUsedPercent  :: !Double
  -- ^ Percentage of total space that is used.
  , diskInfoFreePercent  :: !Double
  -- ^ Percentage of total space available to unprivileged users.
  } deriving (Show, Eq)

emptyDiskUsageInfo :: DiskUsageInfo
emptyDiskUsageInfo = DiskUsageInfo 0 0 0 0 0 0

-- | Query disk usage for the filesystem containing @path@ via @statvfs(2)@.
-- Returns 'emptyDiskUsageInfo' if the call fails.
getDiskUsageInfo :: FilePath -> IO DiskUsageInfo
getDiskUsageInfo path = do
  du <- getDiskUsage path
  let total = diskTotal du
      free  = diskFree du
      avail = diskAvail du
      used  = total - free
      usedPct = if total > 0
                then fromIntegral used * 100.0 / fromIntegral total
                else 0
      freePct = if total > 0
                then fromIntegral avail * 100.0 / fromIntegral total
                else 0
  return DiskUsageInfo
    { diskInfoTotal       = total
    , diskInfoFree        = free
    , diskInfoAvailable   = avail
    , diskInfoUsed        = used
    , diskInfoUsedPercent = usedPct
    , diskInfoFreePercent = freePct
    }

-- --------------------------------------------------------------------------
-- Shared polling channel

newtype DiskUsageChanVar =
  DiskUsageChanVar (TChan DiskUsageInfo, MVar DiskUsageInfo)

-- | Get a broadcast channel that is updated by a shared polling thread.
-- The first call starts the poller; subsequent calls return the same channel.
getDiskUsageInfoChan :: Double -> FilePath -> TaffyIO (TChan DiskUsageInfo)
getDiskUsageInfoChan interval path = do
  DiskUsageChanVar (chan, _) <- setupDiskUsageChanVar interval path
  pure chan

-- | Read the latest cached 'DiskUsageInfo' from the shared poller.
getDiskUsageInfoState :: Double -> FilePath -> TaffyIO DiskUsageInfo
getDiskUsageInfoState interval path = do
  DiskUsageChanVar (_, var) <- setupDiskUsageChanVar interval path
  liftIO $ readMVar var

setupDiskUsageChanVar :: Double -> FilePath -> TaffyIO DiskUsageChanVar
setupDiskUsageChanVar interval path = getStateDefault $ liftIO $ do
  chan <- newBroadcastTChanIO
  info <- getDiskUsageInfo path
  var  <- newMVar info
  void $ forkIO $ forever $ do
    threadDelay (floor $ interval * 1000000)
    catchAny
      (do newInfo <- getDiskUsageInfo path
          void $ swapMVar var newInfo
          atomically $ writeTChan chan newInfo)
      (\e -> logPrintF logName WARNING "DiskUsage poll failed: %s" e)
  pure $ DiskUsageChanVar (chan, var)

logName :: String
logName = "System.Taffybar.Information.DiskUsage"
