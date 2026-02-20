-----------------------------------------------------------------------------

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
-- the process and tracks mode cycling (auto → forced high temp →
-- forced low temp → auto) via @SIGUSR1@.
module System.Taffybar.Information.Wlsunset
  ( -- * Types
    WlsunsetMode (..),
    WlsunsetState (..),
    WlsunsetConfig (..),

    -- * State access
    getWlsunsetChan,
    getWlsunsetState,

    -- * Actions
    cycleWlsunsetMode,
    startWlsunset,
    stopWlsunset,
    toggleWlsunset,
    restartWlsunsetWithTemps,
  )
where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception.Enclosed (catchAny)
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Class
import Data.Default (Default (..))
import Data.List (isPrefixOf)
import System.Log.Logger
import System.Posix.Signals (sigUSR1, signalProcess)
import System.Posix.Types (CPid (..))
import System.Process (readProcess, spawnCommand)
import System.Taffybar.Context
import System.Taffybar.Information.Wakeup (getWakeupChannelSeconds)
import System.Taffybar.Util (logPrintF)
import Text.Read (readMaybe)

-- | The three operating modes that wlsunset cycles through when it
-- receives @SIGUSR1@.  The cycle order matches wlsunset's internal
-- ring: Auto → ForcedHighTemp → ForcedLowTemp → Auto.
data WlsunsetMode
  = -- | Normal day/night schedule
    WlsunsetAuto
  | -- | Forced high (day) temperature — no color shift
    WlsunsetForcedHighTemp
  | -- | Forced low (night) temperature — warm/reddish colors
    WlsunsetForcedLowTemp
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Observable state of the wlsunset process.
data WlsunsetState = WlsunsetState
  { wlsunsetRunning :: Bool,
    wlsunsetMode :: WlsunsetMode,
    -- | The effective high temperature wlsunset is running with.
    wlsunsetEffectiveHighTemp :: Int,
    -- | The effective low temperature wlsunset is running with.
    wlsunsetEffectiveLowTemp :: Int
  }
  deriving (Eq, Show)

-- | Configuration for the wlsunset monitor.
data WlsunsetConfig = WlsunsetConfig
  { -- | Full shell command used to start wlsunset (e.g.
    -- @\"wlsunset -l 38.9 -L -77.0\"@).
    wlsunsetCommand :: String,
    -- | High (day) temperature in Kelvin. Used for display only.
    -- Should match the @-T@ flag passed to wlsunset (default 6500).
    wlsunsetHighTemp :: Int,
    -- | Low (night) temperature in Kelvin. Used for display only.
    -- Should match the @-t@ flag passed to wlsunset (default 4000).
    wlsunsetLowTemp :: Int,
    -- | How often (in seconds) to poll for process status.
    wlsunsetPollIntervalSec :: Int
  }
  deriving (Eq, Show)

instance Default WlsunsetConfig where
  def =
    WlsunsetConfig
      { wlsunsetCommand = "wlsunset",
        wlsunsetHighTemp = 6500,
        wlsunsetLowTemp = 4000,
        wlsunsetPollIntervalSec = 2
      }

-- | Internal state bundle stored in 'contextState' via 'getStateDefault'.
newtype WlsunsetChanVar
  = WlsunsetChanVar (TChan WlsunsetState, MVar WlsunsetState, WlsunsetConfig)

wlsunsetLogPath :: String
wlsunsetLogPath = "System.Taffybar.Information.Wlsunset"

wlsunsetLog :: (MonadIO m) => Priority -> String -> m ()
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
      Just n -> [n]
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
monitorWlsunset ::
  WlsunsetConfig ->
  TaffyIO (TChan WlsunsetState, MVar WlsunsetState, WlsunsetConfig)
monitorWlsunset cfg = do
  let initialState =
        WlsunsetState
          { wlsunsetRunning = False,
            wlsunsetMode = WlsunsetAuto,
            wlsunsetEffectiveHighTemp = wlsunsetHighTemp cfg,
            wlsunsetEffectiveLowTemp = wlsunsetLowTemp cfg
          }
  stateVar <- liftIO $ newMVar initialState
  chan <- liftIO newBroadcastTChanIO
  wakeupChan <- getWakeupChannelSeconds (max 1 (wlsunsetPollIntervalSec cfg))
  ourWakeupChan <- liftIO $ atomically $ dupTChan wakeupChan
  taffyFork $ do
    wlsunsetLog DEBUG "Starting wlsunset polling loop"
    let loop = do
          liftIO $ pollWlsunset chan stateVar
          liftIO $ void $ atomically $ readTChan ourWakeupChan
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
          | not isRunning = WlsunsetAuto
          | otherwise = wlsunsetMode old
        new =
          old
            { wlsunsetRunning = isRunning,
              wlsunsetMode = newMode
            }
    when (new /= old) $ do
      wlsunsetLogF DEBUG "Wlsunset state changed: %s" new
      atomically $ writeTChan chan new
    return new

-- ---------------------------------------------------------------------------
-- Actions
-- ---------------------------------------------------------------------------

-- | Cycle wlsunset mode by sending @SIGUSR1@ to the process.
-- The mode cycles: Auto → ForcedHighTemp → ForcedLowTemp → Auto.
cycleWlsunsetMode :: WlsunsetConfig -> TaffyIO ()
cycleWlsunsetMode cfg = do
  WlsunsetChanVar (chan, var, _) <- getWlsunsetChanVar cfg
  liftIO $ do
    pids <- pgrepWlsunset
    case pids of
      [] -> wlsunsetLog DEBUG "cycleWlsunsetMode: wlsunset not running"
      _ -> do
        mapM_ sendUSR1 pids
        modifyMVar_ var $ \old -> do
          let newMode = case wlsunsetMode old of
                WlsunsetAuto -> WlsunsetForcedHighTemp
                WlsunsetForcedHighTemp -> WlsunsetForcedLowTemp
                WlsunsetForcedLowTemp -> WlsunsetAuto
              new = old {wlsunsetMode = newMode}
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
    _ -> do
      wlsunsetLog DEBUG "Stopping wlsunset (SIGTERM)"
      mapM_ (signalProcess 15) pids

-- | Toggle wlsunset: stop it if running, start it if not.
toggleWlsunset :: WlsunsetConfig -> TaffyIO ()
toggleWlsunset cfg = do
  st <- getWlsunsetState cfg
  if wlsunsetRunning st
    then stopWlsunset cfg
    else startWlsunset cfg

-- | Restart wlsunset with specific low and high temperatures.
-- Kills the running instance, updates the effective temperatures in state,
-- and spawns a new instance with @-t lowTemp -T highTemp@.
restartWlsunsetWithTemps :: WlsunsetConfig -> Int -> Int -> TaffyIO ()
restartWlsunsetWithTemps cfg lowTemp highTemp = do
  WlsunsetChanVar (chan, var, _) <- getWlsunsetChanVar cfg
  liftIO $ do
    pids <- pgrepWlsunset
    mapM_ (signalProcess 15) pids
    let cmd = buildCommandWithTemps (wlsunsetCommand cfg) lowTemp highTemp
    wlsunsetLog DEBUG $ "Restarting wlsunset: " ++ cmd
    void $ spawnCommand cmd
    modifyMVar_ var $ \old -> do
      let new =
            old
              { wlsunsetMode = WlsunsetAuto,
                wlsunsetEffectiveHighTemp = highTemp,
                wlsunsetEffectiveLowTemp = lowTemp
              }
      atomically $ writeTChan chan new
      return new

-- ---------------------------------------------------------------------------
-- Command building
-- ---------------------------------------------------------------------------

-- | Build a wlsunset command with specific temperature flags.
-- Strips any existing @-t@\/@-T@ flags from the base command and appends
-- new ones.
buildCommandWithTemps :: String -> Int -> Int -> String
buildCommandWithTemps baseCmd lowTemp highTemp =
  unwords (stripTempArgs (words baseCmd))
    ++ " -t "
    ++ show lowTemp
    ++ " -T "
    ++ show highTemp

-- | Strip @-t \<val\>@ and @-T \<val\>@ arguments from a word list.
stripTempArgs :: [String] -> [String]
stripTempArgs [] = []
stripTempArgs ("-t" : _ : rest) = stripTempArgs rest
stripTempArgs ("-T" : _ : rest) = stripTempArgs rest
stripTempArgs (x : rest)
  | "-t" `isPrefixOf` x && length x > 2 = stripTempArgs rest
  | "-T" `isPrefixOf` x && length x > 2 = stripTempArgs rest
  | otherwise = x : stripTempArgs rest
