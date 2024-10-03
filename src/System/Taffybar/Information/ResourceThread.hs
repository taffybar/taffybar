{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : System.Taffybar.Information.ResourceThread
Description : Mediates access to a non-threadsafe resource
Copyright   : (c) Rodney Lorrimar, 2024
License     : BSD3-style (see LICENSE)

Maintainer  : Rodney Lorrimar
Stability   : unstable
Portability : GHC

'withResourceThread' mediates access to some resource which must
not be used concurrently by multiple threads.

For example, "Graphics.X11.Xlib" is not thread-safe, so a
'Graphics.X11.Xlib.Display' connection must only be used by one
thread at a time. In this case, we could do:

@
'withResourceThread' (bracket ('openDisplay' d) 'closeDisplay') $ \\r -> do
  window <- ...
  let action = 'mkSafe' $ \\display -> getWMHints display window
  _ <- forkIO $ do
    'resourceRunSync' action r
  'resourceRunSync' action r
@

In addition to serializing access to the resource,
'withResourceThread' ensures that all actions involving the resource
run on the same bound thread. This can help if the resource belongs to
a C library which uses thread-local state.
-}

module System.Taffybar.Information.ResourceThread
  ( withResourceThread
  , ResourceThread
  -- * Running requests safely
  , resourceRunSync
  , resourceRun
  , ResourceUnavailableException(..)
  -- * Building requests
  , SafeReq(..)
  , mkSafe
  -- * Adjusting request execution environment
  , SafeReqParams(..)
  , modSafeReq
  , mkActionModifier
  , mkCancelHandler
  , withTimeout
  , SafeReqTimedOut(..)
  -- * Escape hatches
  , unsafeGetResource
  , resourceThreadCond
  ) where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Unlift (MonadIO(..), MonadUnliftIO(..))
import Control.Concurrent.STM (throwSTM, flushTQueue)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.Log.Logger
import System.Taffybar.Util (labelMyThread)
import Text.Printf (printf)
import UnliftIO.Async (withAsyncBound, Async (..), cancelWith, pollSTM, link)
import UnliftIO.Concurrent (myThreadId)
import UnliftIO.Exception (Exception(..), SomeException, SomeAsyncException, evaluate, bracket_, throwIO, withException, fromEitherM, tryAny, isSyncException, trySyncOrAsync, mask, catchSyncOrAsync, finally, catch)
import UnliftIO.Timeout (timeout)
import UnliftIO.STM

-- | A handle on the resource thread. Use this with 'resourceRun' or
-- 'resourceRunSync' on a 'SafeReq' action.
data ResourceThread x = ResourceThread
  { async :: Async ()
    -- ^ Handle on the resource thread which takes requests from the queue.
  , putRequest :: QueueReq x -> IO ()
    -- ^ Enqueue a request for execution on the resource thread.
  , resource :: x
    -- ^ The protected resource.
  }

instance Show x => Show (ResourceThread x) where
  showsPrec d ctx = showParen (d > 10) $
    showString "ResourceThread { resource = "
    . showsPrec 11 (resource ctx)
    . showString ", async = Async { asyncThreadId = "
    . showsPrec 11 (asyncThreadId (async ctx))
    . showString " }"

-- | Referential equality of protected resource.
instance Eq x => Eq (ResourceThread x) where
  a == b = resource a == resource b

-- | If the resource thread has shut down, or is in the process of
-- shutting down, then 'SafeReq's can't be run. This exception will be
-- thrown or returned for all unfinished requests in the queue, and
-- any new requests.
data ResourceUnavailableException = ResourceUnavailable
  deriving (Show, Generic, Typeable)
instance Exception ResourceUnavailableException

-- | Forks a bound thread for running 'SafeReq's while running the
-- given action.
--
-- A 'ResourceThread' handle is passed to the action.
withResourceThread
  :: (HasCallStack, MonadUnliftIO m)
  => (forall c. (x -> m c) -> m c) -- ^ Resource allocator.
  -> (ResourceThread x -> m a) -- ^ Action to run.
  -> m a
withResourceThread alloc action = do
  ready <- newEmptyTMVarIO
  withAsyncBound (resourceThread (putTMVar ready)) (inner (takeTMVar ready))
  where
    resourceThread put = do
      labelMyThread "resourceThread"
      startResourceQueue alloc put startHandlingRequests
    inner get a = do
      link a
      atomically get >>= action . uncurry (mkResourceThread a)

startResourceQueue
  :: (HasCallStack, MonadUnliftIO m)
  => (forall c. (x -> m c) -> m c) -- ^ Resource allocator.
  -> ((QueueReq x -> STM (), x) -> STM ())
  -> (x -> m (QueueReq x) -> m a)
  -> m a
startResourceQueue withResource ready runQueue = withResource $ \x -> do
    (getReqSTM, flushSTM) <- atomically $ do
      requests <- newTQueue
      ready (writeTQueue requests, x)
      pure (readTQueue requests, flushTQueue requests)
    withRunInIO $ \run ->
      run (runQueue x (atomically getReqSTM))
      `E.finally` (atomically flushSTM >>= mapM_ abortReq)

mkResourceThread :: Async () -> (QueueReq x -> STM ()) -> x -> ResourceThread x
mkResourceThread async writeSTM resource = ResourceThread{..}
  where
    putRequest req = atomically (putRequestSTM req)
      `catch` (\ResourceUnavailable -> abortReq req)
    putRequestSTM req = pollSTM async >>=
      maybe (writeSTM req) (const $ throwSTM ResourceUnavailable)

------------------------------------------------------------------------
-- Queued requests

-- | 'SafeReq' invocations are put on the request queue as @'IO' ()@
-- actions which are passed the resource @x@ as a parameter.
--
-- Almost everything which needs to be done for the 'SafeReq'
-- (e.g. rending back a response) is already baked into the @IO ()@
-- action by 'doSafeRequest'.
--
-- The resource may be 'Nothing', which means that the resource thread
-- is shutting down and so the request should be aborted.
type QueueReq x = Maybe x -> IO ()

-- | The loop which runs 'IO' actions taken from the queue.
startHandlingRequests :: MonadUnliftIO m => x -> m (QueueReq x) -> m a -- ^ Never returns.
startHandlingRequests r getRequest = logAround INFO "startHandlingRequests" $ do
  labelMyThread "resourceRequestThread"
  forever (liftIO . ($ Just r) =<< getRequest)

-- | Returns a 'ResourceUnavailableException' to the callback of the
-- given 'QueueReq'.
abortReq :: MonadIO m => QueueReq x -> m ()
abortReq = liftIO . ($ Nothing)

------------------------------------------------------------------------

-- | Represents 'IO' actions which use the resource and so must
-- be queued and run sequentially using 'resourceRun' or 'resourceRunSync'.
data SafeReq x r = SafeReq
  { safeReqParams :: SafeReqParams x
  , safeReq :: x -> IO r
  }

instance Functor (SafeReq x) where
  fmap f s = s { safeReq = fmap f . safeReq s }

instance Applicative (SafeReq x) where
  pure = SafeReq mempty . const . pure
  f <*> s = s { safeReq = \d -> doSafeRequest f d <*> doSafeRequest s d }

instance Monad (SafeReq x) where
  a >>= f = a { safeReq = \d -> safeReq a d >>= flip doSafeRequest d . f }

instance MonadIO (SafeReq x) where
  liftIO = SafeReq mempty . const

instance MonadUnliftIO (SafeReq x) where
  withRunInIO inner = SafeReq mempty $ \dpy ->
    withRunInIO $ \run ->
    inner (run . flip safeReq dpy)

-- | Construct a 'SafeReq' with default parameters.
mkSafe
  :: (x -> IO r) -- ^ 'IO' action which uses the resource @x@.
  -> SafeReq x r -- ^ A 'SafeReq' with default parameters.
mkSafe = SafeReq mempty

-- | Adjusts 'safeReqParams' of a 'SafeReq' using the given function.
modSafeReq :: (SafeReqParams x -> SafeReqParams x) -> SafeReq x r -> SafeReq x r
modSafeReq f s = s { safeReqParams = f (safeReqParams s) }

-- | 'SafeReqParams' modify the execution environment of a 'SafeReq'.
--
-- 'mempty' represents the default 'SafeReqParams'. The defaults are
-- no 'actionModifier' and a 'cancelHandler' which uses 'cancelWith'.
--
-- They can be combined with @'(<>)'@. When combining 'SafeReqParams'
-- with @p <> q@, the @'actionModifier' q@ runs first (inside of
-- @'actionModifier' p@), and @'cancelHandler' p@ runs before
-- @'cancelHandler' q@.
data SafeReqParams x = SafeReqParams
  { actionModifier :: forall a. x -> IO a -> IO a
  , cancelHandler :: Async () -> SomeAsyncException -> IO ()
  }

instance Semigroup (SafeReqParams x) where
  SafeReqParams f h1 <> SafeReqParams g h2 =
    SafeReqParams (\x -> f x . g x) (\e -> h1 e >> h2 e)

instance Monoid (SafeReqParams x) where
  mempty = SafeReqParams (const id) logCancelWith

logCancelWith :: Async () -> SomeAsyncException  -> IO ()
logCancelWith a e = logAround WARNING ("Default cancelWith handler for " ++ show e)
  (cancelWith a e)

mkActionModifier :: (forall a. x -> IO a -> IO a) -> SafeReqParams x
mkActionModifier f = mempty { actionModifier = f }

mkCancelHandler :: (Async () -> SomeAsyncException -> IO ()) -> SafeReqParams x
mkCancelHandler f = mempty { cancelHandler = f }

------------------------------------------------------------------------

doSafeRequestFull
  :: HasCallStack
  => (Either SomeException a -> IO ()) -- ^ Result callback.
  -> SafeReq x a -- ^ Action which uses the resource @x@
  -> QueueReq x -- ^ A simple 'IO ()' to put on the queue.
doSafeRequestFull callback action = callback <=< doReq
  where
    doReq = trySyncOrAsync . useResource (doSafeRequest action)
    useResource = maybe (throwIO ResourceUnavailable)

-- | Converts a @'SafeReq' x r@ to an @'IO' r@.
doSafeRequest :: HasCallStack => SafeReq x r -> x -> IO r
doSafeRequest action x = logErrors . modifier . doRequest $ action
  where
    doRequest = evaluate <=< flip safeReq x
    modifier = actionModifier (safeReqParams action) x

    enableLogging = True
    logErrors = if enableLogging then flip withException logError else id
    logError :: SomeException -> IO ()
    logError e = when (isSyncException e) $
      pure ()
      -- logHere WARNING $ printf "Handling X11 error: %s\n%s"
      -- (displayException e) (prettyCallStack callStack)

-- | Compare the current thread's ID with that of the designated
-- resource thread. It returns the:
--
--  * the __first__ value if we are running in __some other thread__.
--  * the __second__ value if we are running in __the resource thread__.
resourceThreadCond
  :: (HasCallStack, MonadUnliftIO m)
  => a -- ^ Running on some other thread.
  -> a -- ^ Running on designated resource thread.
  -> ResourceThread x -- ^ Belongs to designated resource thread.
  -> m a
resourceThreadCond isn't itIs r = do
  currentTID <- myThreadId
  let resourceThread = asyncThreadId (async r)
  pure $ if currentTID == resourceThread then itIs else isn't

-- | Post the provided 'SafeReq' action to a 'ResourceThread', and
-- return immediately. The given callback is passed a result upon
-- completion.
resourceRun
  :: HasCallStack
  => SafeReq x a -- ^ Action which uses the resource @x@.
  -> (Either SomeException a -> IO ()) -- ^ Result callback.
  -> ResourceThread x -- ^ Context in which 'SafeReq' action will be run.
  -> IO () -- ^ Returns immediately
resourceRun action callback ctx = do
  run <- resourceThreadCond later now ctx
  liftIO $ run (doSafeRequestFull callback action)
  where
    later = putRequest ctx
    now f = f (Just (resource ctx))

-- | Post the provided 'SafeReq' action to the dedicated resource
-- thread, and wait for the result.
--
-- Any exception caught while running the action will be rethrown here.
--
-- Any asyncronous exception received while waiting for the result
-- will be re-thrown into the dedicated resource thread. This will
-- block until the resource thread has exited. We don't want the
-- resource thread lingering vexatiously while other cleanup handlers
-- are running.
resourceRunSync :: HasCallStack => SafeReq x a -> ResourceThread x -> IO a
resourceRunSync action ctx = do
  var <- newEmptyTMVarIO

  -- Send the action to the resource thread. The completion callback
  -- will fill the MVar.
  fromEitherM $ mask $ \restore -> do
    resourceRun action (atomically . putTMVar var) ctx

    -- Block waiting for resourceRun to run the callback.
    -- We restore the mask to allow cancelling by async exceptions
    -- (although waiting for an empty MVar can always be interrupted).
    restore (atomically $ takeTMVar var)
      `catchSyncOrAsync`
      (restore . runCancelHandler (safeReqParams action) ctx)

-- | An asynchronous exception is received while waiting for the
-- callback.
--
-- Pass the exception to the 'cancelHandler', along with the
-- resource thread 'Async' handle. This allows the caller to
-- make sure the 'SafeReq' action is properly stopped, before
-- unblocking.
--
-- Any exception encountered while running 'cancelHandler' is ignored,
-- in favour of the first exception received.
runCancelHandler :: HasCallStack => SafeReqParams x -> ResourceThread x -> SomeAsyncException -> IO a
runCancelHandler params ctx e = do
  logHere NOTICE $ "Caught async exception while waiting for result of SafeReq: " ++ show e
  logAround NOTICE "Running cancelHandler" $
    -- This will usually block until thread exits
    void $ tryAny $ cancelHandler params (async ctx) e
  logHere DEBUG "Cancel finished, rethrowing the async exception"
  throwIO e

-- | Use this if you need access to the allocated resource outside of 'resourceRun'.
unsafeGetResource :: ResourceThread x -> x
unsafeGetResource = resource

------------------------------------------------------------------------

-- | This can be added to 'SafeReqParams' set a timeout for the
-- action.
--
-- Warnings:
--
--   1. Foreign function calls usually can't be interrupted by an
--   asynchronous exception. If they have an @interruptible@
--   annotation, then interruption /might/ be possible, depending on
--   specific implementation details of the foreign function.
--
--   2. After cancelling an operation, the resource may be left in a
--   state where it is unable to immediately run the next queued
--   action. This also depends on specific implementation details.
withTimeout
  :: Int -- ^ Interval in microseconds.
  -> SafeReqParams x
withTimeout t = mempty { actionModifier = const $ throwTimeout t }

throwTimeout :: MonadUnliftIO m => Int -> m a -> m a
throwTimeout t = maybe (throwIO (SafeReqTimedOut t)) pure <=< timeout t

-- | An 'Exception' which is thrown if the 'withTimeout' interval is reached.
newtype SafeReqTimedOut = SafeReqTimedOut { requestTimeoutUsec :: Int }
  deriving (Show, Read, Eq, Typeable, Generic)

instance Exception SafeReqTimedOut where
  displayException (SafeReqTimedOut t) = printf "call timed out after %dusec" t

------------------------------------------------------------------------

logHere :: MonadIO m => Priority -> String -> m ()
logHere p = liftIO . logM "System.Taffybar.Information.ResourceThread" p

logAround :: MonadUnliftIO m => Priority -> String -> m a -> m a
logAround p msg = bracket_ (logHere p (msg ++ "...")) (logHere p ("..." ++ msg))
