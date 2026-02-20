{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : System.Taffybar.Information.Wakeup
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Shared coordinated wakeup channels keyed by polling interval.
--
-- This module provides a single scheduler thread that wakes registered
-- intervals from a shared monotonic timeline. Intervals that evenly divide each
-- other naturally align to the same schedule (e.g. 10s wakeups occur on every
-- other 5s boundary). The wall-clock phase is chosen from the Unix epoch, which
-- means wakeups are top-of-second aligned and minute-start aligned whenever the
-- interval permits.
module System.Taffybar.Information.Wakeup
  ( WakeupEvent (..),
    WakeupSchedulerEvent (..),
    WakeupChannel (..),
    taffyForeverWithDelay,
    getWakeupChannelNanoseconds,
    getWakeupChannelSeconds,
    getWakeupChannelForDelay,
    getWakeupChannel,
    getWakeupSchedulerEvents,
    getRegisteredWakeupIntervalsNanoseconds,
    intervalSecondsToNanoseconds,
    nextWallAlignedWakeupNs,
    nextAlignedWakeupNs,
    intervalDueAtStepNs,
  )
where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, readTChan)
import Control.Exception.Enclosed (catchAny)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, asks, runReaderT)
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import GHC.TypeNats (KnownNat, Nat, natVal)
import System.Log.Logger (Priority (..), logM)
import System.Taffybar.Context (Context, TaffyIO, wakeupManager)
import System.Taffybar.Information.Wakeup.Manager
  ( WakeupEvent (..),
    WakeupSchedulerEvent (..),
    WakeupManager,
    intervalDueAtStepNs,
    intervalSecondsToNanoseconds,
    nextAlignedWakeupNs,
    nextWallAlignedWakeupNs,
    registerWakeupInterval,
    secondsToNanoseconds,
    subscribeWakeupSchedulerEvents,
  )
import qualified System.Taffybar.Information.Wakeup.Manager as WakeupManager
import Text.Printf (printf)

-- | Type-tagged wakeup channel for @TypeApplications@-based lookup.
newtype WakeupChannel (seconds :: Nat)
  = WakeupChannel {unWakeupChannel :: TChan WakeupEvent}

-- | Execute a 'TaffyIO' action on a shared synchronized wakeup schedule.
--
-- This is the preferred context-aware replacement for
-- 'System.Taffybar.Util.foreverWithDelay'.
taffyForeverWithDelay :: (RealFrac d) => d -> TaffyIO () -> TaffyIO ThreadId
taffyForeverWithDelay delay action = do
  wakeupChan <- getWakeupChannelForDelay delay
  ourWakeupChan <- liftIO $ atomically $ dupTChan wakeupChan
  context <- ask
  liftIO $
    forkIO $
      forever $ do
        safeAction context
        void $ atomically $ readTChan ourWakeupChan
  where
    safeAction :: Context -> IO ()
    safeAction context =
      catchAny
        (runReaderT action context)
        (logM logPath WARNING . printf "Error in taffyForeverWithDelay %s" . show)

-- | Return the shared wakeup channel for an interval in nanoseconds.
getWakeupChannelNanoseconds :: Word64 -> TaffyIO (TChan WakeupEvent)
getWakeupChannelNanoseconds intervalNs = do
  manager <- getWakeupManager
  liftIO $ registerWakeupInterval manager intervalNs

-- | Subscribe to aggregated scheduler wakeup events.
getWakeupSchedulerEvents :: TaffyIO (TChan WakeupSchedulerEvent)
getWakeupSchedulerEvents = do
  manager <- getWakeupManager
  liftIO $ subscribeWakeupSchedulerEvents manager

-- | Return all currently registered wakeup intervals (nanoseconds).
getRegisteredWakeupIntervalsNanoseconds :: TaffyIO [Word64]
getRegisteredWakeupIntervalsNanoseconds = do
  manager <- getWakeupManager
  liftIO $ WakeupManager.getRegisteredWakeupIntervalsNanoseconds manager

-- | Return the shared wakeup channel for @seconds@.
getWakeupChannelSeconds :: Int -> TaffyIO (TChan WakeupEvent)
getWakeupChannelSeconds seconds =
  case secondsToNanoseconds seconds of
    Left err -> fail err
    Right intervalNs -> getWakeupChannelNanoseconds intervalNs

-- | Return the shared wakeup channel for an interval expressed in seconds.
getWakeupChannelForDelay :: (RealFrac d) => d -> TaffyIO (TChan WakeupEvent)
getWakeupChannelForDelay seconds =
  case intervalSecondsToNanoseconds seconds of
    Left err -> fail err
    Right intervalNs -> getWakeupChannelNanoseconds intervalNs

-- | Type-driven variant of 'getWakeupChannelSeconds'.
--
-- Example:
--
-- @
-- wake5 <- getWakeupChannel @5
-- wake10 <- getWakeupChannel @10
-- @
getWakeupChannel ::
  forall seconds.
  (KnownNat seconds) =>
  TaffyIO (TChan WakeupEvent)
getWakeupChannel = do
  seconds <- intervalSecondsFromType @seconds
  getWakeupChannelSeconds seconds

getWakeupManager :: TaffyIO WakeupManager
getWakeupManager = asks wakeupManager

intervalSecondsFromType :: forall seconds m. (KnownNat seconds, MonadFail m) => m Int
intervalSecondsFromType =
  let secondsInteger = toInteger (natVal (Proxy :: Proxy seconds))
      maxIntInteger = toInteger (maxBound :: Int)
   in if
        | secondsInteger <= 0 ->
            fail "Wakeup interval must be greater than 0 seconds"
        | secondsInteger > maxIntInteger ->
            fail "Wakeup interval does not fit into an Int"
        | otherwise ->
            return (fromInteger secondsInteger)

logPath :: String
logPath = "System.Taffybar.Information.Wakeup"
