-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.Wlsunset
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides process-level management of @wlsunset@, a
-- Wayland day\/night gamma adjustor. It polls for the running state of
-- the process and tracks mode cycling (auto → forced-warm → forced-cool
-- → auto) via @SIGUSR1@.
-----------------------------------------------------------------------------
module System.Taffybar.Information.Wlsunset
  ( -- * Types
    WlsunsetMode(..)
  , WlsunsetState(..)
  , WlsunsetConfig(..)
    -- * State access
  , getWlsunsetChan
  , getWlsunsetState
    -- * Actions
  , cycleWlsunsetMode
  , startWlsunset
  , stopWlsunset
  , toggleWlsunset
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TChan
import           Control.Exception.Enclosed (catchAny)
import           Control.Monad (void, when)
import           Control.Monad.IO.Class
import           Control.Monad.STM (atomically)
import           Control.Monad.Trans.Class
import           Data.Default (Default(..))
import           System.Log.Logger
import           System.Posix.Signals (signalProcess, sigUSR1)
import           System.Posix.Types (CPid(..))
import           System.Process (readProcess, spawnCommand)
import           System.Taffybar.Context
import           System.Taffybar.Util (logPrintF)
import           Text.Read (readMaybe)

-- | The three operating modes that wlsunset cycles through when it
-- receives @SIGUSR1@.
data WlsunsetMode
  = WlsunsetAuto        -- ^ Normal day/night schedule
  | WlsunsetForcedWarm  -- ^ Forced warm (night) temperature
  | WlsunsetForcedCool  -- ^ Forced cool (day) temperature
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Observable state of the wlsunset process.
data WlsunsetState = WlsunsetState
  { wlsunsetRunning :: Bool
  , wlsunsetMode    :: WlsunsetMode
  } deriving (Eq, Show)

-- | Configuration for the wlsunset monitor.
data WlsunsetConfig = WlsunsetConfig
  { -- | Full shell command used to start wlsunset (e.g.
    -- @\"wlsunset -l 38.9 -L -77.0\"@).
    wlsunsetCommand        :: String
    -- | How often (in seconds) to poll for process status.
  , wlsunsetPollIntervalSec :: Int
  } deriving (Eq, Show)

instance Default WlsunsetConfig where
  def = WlsunsetConfig
    { wlsunsetCommand         = "wlsunset"
    , wlsunsetPollIntervalSec = 2
    }

-- | Internal state bundle stored in 'contextState' via 'getStateDefault'.
newtype WlsunsetChanVar =
  WlsunsetChanVar (TChan WlsunsetState, MVar WlsunsetState, WlsunsetConfig)

wlsunsetLogPath :: String
wlsunsetLogPath = "System.Taffybar.Information.Wlsunset"

wlsunsetLog :: MonadIO m => Priority -> String -> m ()
wlsunsetLog priority = liftIO . logM wlsunsetLogPath priority

wlsunsetLogF :: (MonadIO m, Show t) => Priority -> String -> t -> m ()
wlsunsetLogF = logPrintF wlsunsetLogPath

-- ---------------------------------------------------------------------------
-- Process helpers
-- ---------------------------------------------------------------------------

-- | Check whether wlsunset is running by calling @pgrep -x wlsunset@.
-- Returns a list of matching PIDs (empty when not running).
pgrepWlsunset :: IO [CPid]
pgrepWlsunset =
  (parsePids <$> readProcess "pgrep" ["-x", "wlsunset"] "")
    `catchAny` (\_ -> return [])
  where
    parsePids = map (CPid . fromIntegral) . concatMap toList . lines
    toList s = case (readMaybe s :: Maybe Int) of
      Just n  -> [n]
      Nothing -> []

-- | Send @SIGUSR1@ to a wlsunset process to cycle its mode.
sendUSR1 :: CPid -> IO ()
sendUSR1 = signalProcess sigUSR1

-- ---------------------------------------------------------------------------
-- State management
-- ---------------------------------------------------------------------------

getWlsunsetChanVar :: WlsunsetConfig -> TaffyIO WlsunsetChanVar
getWlsunsetChanVar cfg =
  getStateDefault $ WlsunsetChanVar <$> monitorWlsunset cfg

-- | Get a broadcast channel that receives 'WlsunsetState' updates.
getWlsunsetChan :: WlsunsetConfig -> TaffyIO (TChan WlsunsetState)
getWlsunsetChan cfg = do
  WlsunsetChanVar (chan, _, _) <- getWlsunsetChanVar cfg
  return chan

-- | Get the current 'WlsunsetState'.
getWlsunsetState :: WlsunsetConfig -> TaffyIO WlsunsetState
getWlsunsetState cfg = do
  WlsunsetChanVar (_, var, _) <- getWlsunsetChanVar cfg
  lift $ readMVar var

-- | Start the polling loop that monitors wlsunset.
monitorWlsunset
  :: WlsunsetConfig
  -> TaffyIO (TChan WlsunsetState, MVar WlsunsetState, WlsunsetConfig)
monitorWlsunset cfg = do
  let initialState = WlsunsetState { wlsunsetRunning = False
                                   , wlsunsetMode    = WlsunsetAuto
                                   }
  stateVar <- liftIO $ newMVar initialState
  chan     <- liftIO newBroadcastTChanIO
  taffyFork $ do
    wlsunsetLog DEBUG "Starting wlsunset polling loop"
    let loop = do
          liftIO $ pollWlsunset chan stateVar
          liftIO $ threadDelay (wlsunsetPollIntervalSec cfg * 1000000)
          loop
    loop
  return (chan, stateVar, cfg)

-- | A single poll iteration: check process status, update state, and
-- broadcast if changed.
pollWlsunset :: TChan WlsunsetState -> MVar WlsunsetState -> IO ()
pollWlsunset chan var = do
  pids <- pgrepWlsunset
  let isRunning = not (null pids)
  modifyMVar_ var $ \old -> do
    let wasRunning = wlsunsetRunning old
        -- When the process freshly appears, reset mode to Auto.
        newMode
          | not wasRunning && isRunning = WlsunsetAuto
          | not isRunning              = WlsunsetAuto
          | otherwise                  = wlsunsetMode old
        new = WlsunsetState { wlsunsetRunning = isRunning
                             , wlsunsetMode    = newMode
                             }
    when (new /= old) $ do
      wlsunsetLogF DEBUG "Wlsunset state changed: %s" new
      atomically $ writeTChan chan new
    return new

-- ---------------------------------------------------------------------------
-- Actions
-- ---------------------------------------------------------------------------

-- | Cycle wlsunset mode by sending @SIGUSR1@ to the process.
-- The mode cycles: Auto → ForcedWarm → ForcedCool → Auto.
cycleWlsunsetMode :: WlsunsetConfig -> TaffyIO ()
cycleWlsunsetMode cfg = do
  WlsunsetChanVar (chan, var, _) <- getWlsunsetChanVar cfg
  liftIO $ do
    pids <- pgrepWlsunset
    case pids of
      [] -> wlsunsetLog DEBUG "cycleWlsunsetMode: wlsunset not running"
      _  -> do
        mapM_ sendUSR1 pids
        modifyMVar_ var $ \old -> do
          let newMode = case wlsunsetMode old of
                WlsunsetAuto       -> WlsunsetForcedWarm
                WlsunsetForcedWarm -> WlsunsetForcedCool
                WlsunsetForcedCool -> WlsunsetAuto
              new = old { wlsunsetMode = newMode }
          wlsunsetLogF DEBUG "Cycled wlsunset mode: %s" newMode
          atomically $ writeTChan chan new
          return new

-- | Start the wlsunset process using the configured command.
startWlsunset :: WlsunsetConfig -> TaffyIO ()
startWlsunset cfg = liftIO $ do
  wlsunsetLog DEBUG $ "Starting wlsunset: " ++ wlsunsetCommand cfg
  void $ spawnCommand (wlsunsetCommand cfg)

-- | Stop wlsunset by sending @SIGTERM@ (signal 15) to all instances.
stopWlsunset :: WlsunsetConfig -> TaffyIO ()
stopWlsunset _cfg = liftIO $ do
  pids <- pgrepWlsunset
  case pids of
    [] -> wlsunsetLog DEBUG "stopWlsunset: wlsunset not running"
    _  -> do
      wlsunsetLog DEBUG "Stopping wlsunset (SIGTERM)"
      mapM_ (signalProcess 15) pids

-- | Toggle wlsunset: stop it if running, start it if not.
toggleWlsunset :: WlsunsetConfig -> TaffyIO ()
toggleWlsunset cfg = do
  st <- getWlsunsetState cfg
  if wlsunsetRunning st
    then stopWlsunset cfg
    else startWlsunset cfg
