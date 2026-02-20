-- |
-- Module      : System.Taffybar.Information.Wakeup.Manager
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Internal shared wakeup manager implementation.
module System.Taffybar.Information.Wakeup.Manager
  ( WakeupEvent (..),
    WakeupSchedulerEvent (..),
    WakeupManager,
    newWakeupManager,
    registerWakeupInterval,
    subscribeWakeupSchedulerEvents,
    getRegisteredWakeupIntervalsNanoseconds,
    secondsToNanoseconds,
    intervalSecondsToNanoseconds,
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
    dupTChan,
    newBroadcastTChan,
    newBroadcastTChanIO,
    newTChanIO,
    readTChan,
    tryReadTChan,
    writeTChan,
  )
import Control.Exception.Enclosed (catchAny)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Log.Logger (Priority (..), logM)
import Text.Printf (printf)

-- | A wakeup event emitted for a registered interval.
data WakeupEvent = WakeupEvent
  { wakeupIntervalNanoseconds :: !Word64,
    -- | Monotonic counter for this interval. If the scheduler was delayed,
    -- this counter can advance by more than one between events.
    wakeupTickCount :: !Word64
  }
  deriving (Eq, Show)

-- | A scheduler event containing all interval wakeups published together.
data WakeupSchedulerEvent = WakeupSchedulerEvent
  { wakeupEventTimeNanoseconds :: !Word64,
    wakeupDueEvents :: ![WakeupEvent]
  }
  deriving (Eq, Show)

-- Internal scheduler state for one interval.
data IntervalRegistration = IntervalRegistration
  { intervalNanoseconds :: !Word64,
    intervalChannel :: TChan WakeupEvent,
    intervalNextDueNs :: !Word64,
    intervalTickCount :: !Word64
  }

-- Shared wakeup manager state.
data WakeupManager = WakeupManager
  { -- Approximate wall clock as @monotonic + wakeupRealtimeOffsetNs@.
    wakeupRealtimeOffsetNs :: !Integer,
    wakeupIntervals :: TVar (M.Map Word64 IntervalRegistration),
    wakeupRescheduleChan :: TChan (),
    wakeupDebugChan :: TChan WakeupSchedulerEvent
  }

newWakeupManager :: IO WakeupManager
newWakeupManager = do
  realtimeOffsetNs <- getRealtimeOffsetNs
  intervalsVar <- newTVarIO M.empty
  rescheduleChan <- newTChanIO
  debugChan <- newBroadcastTChanIO
  let manager =
        WakeupManager
          { wakeupRealtimeOffsetNs = realtimeOffsetNs,
            wakeupIntervals = intervalsVar,
            wakeupRescheduleChan = rescheduleChan,
            wakeupDebugChan = debugChan
          }
  _ <- forkIO (runWakeupScheduler manager)
  pure manager

registerWakeupInterval :: WakeupManager -> Word64 -> IO (TChan WakeupEvent)
registerWakeupInterval manager intervalNs =
  case validateIntervalNanoseconds intervalNs of
    Left err -> ioError (userError err)
    Right validIntervalNs -> do
      now <- getMonotonicTimeNSec
      atomically $ do
        registrations <- readTVar (wakeupIntervals manager)
        case M.lookup validIntervalNs registrations of
          Just registration -> pure (intervalChannel registration)
          Nothing -> do
            channel <- newBroadcastTChan
            let registration =
                  IntervalRegistration
                    { intervalNanoseconds = validIntervalNs,
                      intervalChannel = channel,
                      intervalNextDueNs =
                        nextWallAlignedWakeupNs
                          (wakeupRealtimeOffsetNs manager)
                          validIntervalNs
                          now,
                      intervalTickCount = 0
                    }
            writeTVar
              (wakeupIntervals manager)
              (M.insert validIntervalNs registration registrations)
            -- Ensure the scheduler recomputes immediately, so newly added short
            -- intervals are not blocked behind an older long sleep.
            writeTChan (wakeupRescheduleChan manager) ()
            pure channel

subscribeWakeupSchedulerEvents :: WakeupManager -> IO (TChan WakeupSchedulerEvent)
subscribeWakeupSchedulerEvents manager =
  atomically $ dupTChan (wakeupDebugChan manager)

getRegisteredWakeupIntervalsNanoseconds :: WakeupManager -> IO [Word64]
getRegisteredWakeupIntervalsNanoseconds manager =
  atomically $ M.keys <$> readTVar (wakeupIntervals manager)

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
      logM logPath WARNING $ printf "Wakeup scheduler crashed and will be restarted: %s" (show e)
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
    updatedWithEvents <- mapM (advanceIntervalIfDue nowNs) registrations
    let updated = fst <$> updatedWithEvents
        dueEvents = mapMaybe snd (M.elems updatedWithEvents)
    writeTVar (wakeupIntervals manager) updated
    if null dueEvents
      then pure ()
      else
        writeTChan
          (wakeupDebugChan manager)
          WakeupSchedulerEvent
            { wakeupEventTimeNanoseconds = nowNs,
              wakeupDueEvents = dueEvents
            }

advanceIntervalIfDue :: Word64 -> IntervalRegistration -> STM (IntervalRegistration, Maybe WakeupEvent)
advanceIntervalIfDue nowNs registration
  | intervalNextDueNs registration > nowNs = pure (registration, Nothing)
  | otherwise = do
      let intervalsElapsed =
            ((nowNs - intervalNextDueNs registration) `div` intervalNanoseconds registration) + 1
          nextDueNs' =
            fromInteger $
              toInteger (intervalNextDueNs registration)
                + toInteger intervalsElapsed * toInteger (intervalNanoseconds registration)
          tickCount = intervalTickCount registration + intervalsElapsed
          wakeupEvent =
            WakeupEvent
              { wakeupIntervalNanoseconds = intervalNanoseconds registration,
                wakeupTickCount = tickCount
              }
      writeTChan
        (intervalChannel registration)
        wakeupEvent
      pure
        ( registration
            { intervalNextDueNs = nextDueNs',
              intervalTickCount = tickCount
            },
          Just wakeupEvent
        )

nextDueNs :: M.Map Word64 IntervalRegistration -> Maybe Word64
nextDueNs registrations
  | M.null registrations = Nothing
  | otherwise = Just $ minimum $ intervalNextDueNs <$> M.elems registrations

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

-- | Convert an interval in seconds to nanoseconds.
intervalSecondsToNanoseconds :: (RealFrac d) => d -> Either String Word64
intervalSecondsToNanoseconds seconds
  | secondsRational <= 0 = Left "Wakeup interval must be greater than 0 seconds"
  | intervalNanosecondsInteger <= 0 = Left "Wakeup interval rounded to zero nanoseconds"
  | intervalNanosecondsInteger > maxWord64Integer = Left "Wakeup interval is too large"
  | otherwise = Right (fromInteger intervalNanosecondsInteger)
  where
    secondsRational = toRational seconds
    intervalNanosecondsInteger = round (secondsRational * 1000000000)
    maxWord64Integer = toInteger (maxBound :: Word64)

validateIntervalNanoseconds :: Word64 -> Either String Word64
validateIntervalNanoseconds intervalNs
  | intervalNs == 0 = Left "Wakeup interval must be greater than 0 nanoseconds"
  | otherwise = Right intervalNs

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
    Nothing -> pure ()
    Just () -> drainRescheduleChan chan

getRealtimeOffsetNs :: IO Integer
getRealtimeOffsetNs = do
  monotonicBefore <- getMonotonicTimeNSec
  wallNow <- getPOSIXTimeNanoseconds
  monotonicAfter <- getMonotonicTimeNSec
  let monotonicMidpoint =
        (toInteger monotonicBefore + toInteger monotonicAfter) `div` 2
  pure $ wallNow - monotonicMidpoint

getPOSIXTimeNanoseconds :: IO Integer
getPOSIXTimeNanoseconds = do
  -- POSIX time has picosecond precision; convert to nanoseconds.
  posix <- getPOSIXTime
  let picoseconds :: Integer
      picoseconds = round (posix * 1000000000000)
  pure (picoseconds `div` 1000)

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
