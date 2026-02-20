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
    WakeupChannel (..),
    getWakeupChannelSeconds,
    getWakeupChannel,
    nextWallAlignedWakeupNs,
    nextAlignedWakeupNs,
    intervalDueAtStepNs,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    check,
    newTVarIO,
    orElse,
    readTVar,
    registerDelay,
    writeTVar,
  )
import Control.Concurrent.STM.TChan
  ( TChan,
    newBroadcastTChan,
    newTChanIO,
    readTChan,
    tryReadTChan,
    writeTChan,
  )
import Control.Exception.Enclosed (catchAny)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.TypeNats (KnownNat, Nat, natVal)
import System.Log.Logger (Priority (..))
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Util (logPrintF)

-- | A wakeup event emitted for a registered interval.
data WakeupEvent = WakeupEvent
  { wakeupIntervalSeconds :: !Int,
    -- | Monotonic counter for this interval. If the scheduler was delayed,
    -- this counter can advance by more than one between events.
    wakeupTickCount :: !Word64
  }
  deriving (Eq, Show)

-- | Type-tagged wakeup channel for @TypeApplications@-based lookup.
newtype WakeupChannel (seconds :: Nat)
  = WakeupChannel {unWakeupChannel :: TChan WakeupEvent}

-- Internal scheduler state for one interval.
data IntervalRegistration = IntervalRegistration
  { intervalSeconds :: !Int,
    intervalNanoseconds :: !Word64,
    intervalChannel :: TChan WakeupEvent,
    intervalNextDueNs :: !Word64,
    intervalTickCount :: !Word64
  }

-- Shared wakeup manager state.
data WakeupManager = WakeupManager
  { -- Approximate wall clock as @monotonic + wakeupRealtimeOffsetNs@.
    wakeupRealtimeOffsetNs :: !Integer,
    wakeupIntervals :: TVar (M.Map Int IntervalRegistration),
    wakeupRescheduleChan :: TChan ()
  }

-- | Return the shared wakeup channel for @seconds@. The first call registers the
-- interval with the manager; subsequent calls reuse the same channel.
getWakeupChannelSeconds :: Int -> TaffyIO (TChan WakeupEvent)
getWakeupChannelSeconds seconds = do
  manager <- getWakeupManager
  liftIO $ registerInterval manager seconds

-- | Type-driven variant of 'getWakeupChannelSeconds'.
--
-- Example:
--
-- @
-- wake5 <- getWakeupChannel @5
-- wake10 <- getWakeupChannel @10
-- @
getWakeupChannel :: forall seconds. (KnownNat seconds) => TaffyIO (TChan WakeupEvent)
getWakeupChannel = do
  (WakeupChannel chan :: WakeupChannel seconds) <-
    getStateDefault $ do
      seconds <- intervalSecondsFromType @seconds
      WakeupChannel <$> getWakeupChannelSeconds seconds
  return chan

getWakeupManager :: TaffyIO WakeupManager
getWakeupManager = getStateDefault $ liftIO $ do
  realtimeOffsetNs <- getRealtimeOffsetNs
  intervalsVar <- newTVarIO M.empty
  rescheduleChan <- newTChanIO
  let manager =
        WakeupManager
          { wakeupRealtimeOffsetNs = realtimeOffsetNs,
            wakeupIntervals = intervalsVar,
            wakeupRescheduleChan = rescheduleChan
          }
  _ <- forkIO (runWakeupScheduler manager)
  return manager

registerInterval :: WakeupManager -> Int -> IO (TChan WakeupEvent)
registerInterval manager seconds =
  case secondsToNanoseconds seconds of
    Left err -> ioError (userError err)
    Right intervalNs -> do
      now <- getMonotonicTimeNSec
      atomically $ do
        registrations <- readTVar (wakeupIntervals manager)
        case M.lookup seconds registrations of
          Just registration -> return (intervalChannel registration)
          Nothing -> do
            channel <- newBroadcastTChan
            let registration =
                  IntervalRegistration
                    { intervalSeconds = seconds,
                      intervalNanoseconds = intervalNs,
                      intervalChannel = channel,
                      intervalNextDueNs =
                        nextWallAlignedWakeupNs
                          (wakeupRealtimeOffsetNs manager)
                          intervalNs
                          now,
                      intervalTickCount = 0
                    }
            writeTVar
              (wakeupIntervals manager)
              (M.insert seconds registration registrations)
            -- Ensure the scheduler recomputes immediately, so newly added short
            -- intervals are not blocked behind an older long sleep.
            writeTChan (wakeupRescheduleChan manager) ()
            return channel

runWakeupScheduler :: WakeupManager -> IO ()
runWakeupScheduler manager =
  schedulerLoop `catchAny` onSchedulerException
  where
    schedulerLoop = do
      maybeNextDue <- atomically $ nextDueNs <$> readTVar (wakeupIntervals manager)
      case maybeNextDue of
        Nothing -> atomically (readTChan (wakeupRescheduleChan manager) >> drainRescheduleChan (wakeupRescheduleChan manager))
        Just nextDue -> do
          now <- getMonotonicTimeNSec
          if nextDue <= now
            then publishDueIntervals manager now
            else waitForRescheduleOrTimeout manager (nextDue - now)
      schedulerLoop

    onSchedulerException e = do
      logPrintF logPath WARNING "Wakeup scheduler crashed and will be restarted: %s" e
      threadDelay 1000000
      runWakeupScheduler manager

waitForRescheduleOrTimeout :: WakeupManager -> Word64 -> IO ()
waitForRescheduleOrTimeout manager delayNs = do
  timeoutVar <- registerDelay (toRegisterDelayMicros delayNs)
  atomically $
    (readTChan (wakeupRescheduleChan manager) >> drainRescheduleChan (wakeupRescheduleChan manager))
      `orElse` (readTVar timeoutVar >>= check)

publishDueIntervals :: WakeupManager -> Word64 -> IO ()
publishDueIntervals manager nowNs =
  atomically $ do
    registrations <- readTVar (wakeupIntervals manager)
    updated <- mapM (advanceIntervalIfDue nowNs) registrations
    writeTVar (wakeupIntervals manager) updated

advanceIntervalIfDue :: Word64 -> IntervalRegistration -> STM IntervalRegistration
advanceIntervalIfDue nowNs registration
  | intervalNextDueNs registration > nowNs = return registration
  | otherwise = do
      let intervalsElapsed =
            ((nowNs - intervalNextDueNs registration) `div` intervalNanoseconds registration) + 1
          nextDueNs' =
            fromInteger $
              toInteger (intervalNextDueNs registration)
                + toInteger intervalsElapsed * toInteger (intervalNanoseconds registration)
          tickCount = intervalTickCount registration + intervalsElapsed
      writeTChan
        (intervalChannel registration)
        WakeupEvent
          { wakeupIntervalSeconds = intervalSeconds registration,
            wakeupTickCount = tickCount
          }
      return
        registration
          { intervalNextDueNs = nextDueNs',
            intervalTickCount = tickCount
          }

nextDueNs :: M.Map Int IntervalRegistration -> Maybe Word64
nextDueNs registrations
  | M.null registrations = Nothing
  | otherwise = Just $ minimum $ intervalNextDueNs <$> M.elems registrations

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

secondsToNanoseconds :: Int -> Either String Word64
secondsToNanoseconds seconds
  | seconds <= 0 = Left "Wakeup interval must be greater than 0 seconds"
  | secondsInteger > maxWord64Integer =
      Left "Wakeup interval is too large"
  | otherwise =
      Right (fromInteger secondsInteger)
  where
    secondsInteger = toInteger seconds * 1000000000
    maxWord64Integer = toInteger (maxBound :: Word64)

-- | Convert a delay in nanoseconds into a value suitable for
-- 'Control.Concurrent.STM.registerDelay'.
toRegisterDelayMicros :: Word64 -> Int
toRegisterDelayMicros nanoseconds =
  fromInteger (min maxIntInteger micros)
  where
    micros = (toInteger nanoseconds + 999) `div` 1000
    maxIntInteger = toInteger (maxBound :: Int)

drainRescheduleChan :: TChan () -> STM ()
drainRescheduleChan chan = do
  maybeEvent <- tryReadTChan chan
  case maybeEvent of
    Nothing -> return ()
    Just () -> drainRescheduleChan chan

getRealtimeOffsetNs :: IO Integer
getRealtimeOffsetNs = do
  monotonicBefore <- getMonotonicTimeNSec
  wallNow <- getPOSIXTimeNanoseconds
  monotonicAfter <- getMonotonicTimeNSec
  let monotonicMidpoint =
        (toInteger monotonicBefore + toInteger monotonicAfter) `div` 2
  return $ wallNow - monotonicMidpoint

getPOSIXTimeNanoseconds :: IO Integer
getPOSIXTimeNanoseconds = do
  -- POSIX time has picosecond precision; convert to nanoseconds.
  posix <- getPOSIXTime
  let picoseconds :: Integer
      picoseconds = round (posix * 1000000000000)
  return (picoseconds `div` 1000)

-- | Return the next due monotonic timestamp strictly after @nowNs@ for the
-- given @intervalNs@, aligned to wall-clock interval boundaries.
--
-- The wall clock is approximated as @monotonic + realtimeOffsetNs@.
nextWallAlignedWakeupNs :: Integer -> Word64 -> Word64 -> Word64
nextWallAlignedWakeupNs realtimeOffsetNs intervalNs nowNs =
  fromInteger (toInteger nowNs + delta)
  where
    wallNowNs = toInteger nowNs + realtimeOffsetNs
    intervalInteger = toInteger intervalNs
    remainder = wallNowNs `mod` intervalInteger
    delta
      | remainder == 0 = intervalInteger
      | otherwise = intervalInteger - remainder

-- | Return the @step@-th due timestamp for @intervalNs@ from @anchorNs@.
--
-- Steps are 1-indexed (step 1 is the first due time at @anchor + interval@).
intervalDueAtStepNs :: Word64 -> Word64 -> Word64 -> Word64
intervalDueAtStepNs anchorNs intervalNs step =
  fromInteger $
    toInteger anchorNs
      + toInteger intervalNs * toInteger step

-- | Return the next due timestamp strictly after @nowNs@ for the given
-- @intervalNs@ from @anchorNs@.
nextAlignedWakeupNs :: Word64 -> Word64 -> Word64 -> Word64
nextAlignedWakeupNs anchorNs intervalNs nowNs =
  intervalDueAtStepNs anchorNs intervalNs (stepsElapsed + 1)
  where
    stepsElapsed
      | nowNs <= anchorNs = 0
      | otherwise = (nowNs - anchorNs) `div` intervalNs

logPath :: String
logPath = "System.Taffybar.Information.Wakeup"
