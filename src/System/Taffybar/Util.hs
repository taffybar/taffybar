{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Util
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------

module System.Taffybar.Util (
  -- * Configuration
    taffyStateDir
  -- * GTK concurrency
  , module Gtk
  -- * GLib
  , catchGErrorsAsLeft
  -- * Logging
  , logPrintF
  -- * Text
  , truncateString
  , truncateText
  -- * Resources
  , downloadURIToPath
  , getPixbufFromFilePath
  , safePixbufNewFromFile
  -- * Logic Combinators
  , (<||>)
  , (<|||>)
  , forkM
  , ifM
  , anyM
  , maybeTCombine
  , maybeToEither
  -- * Control
  , foreverWithVariableDelay
  , foreverWithDelay
  -- * Process control
  , runCommand
  , onSigINT
  , maybeHandleSigHUP
  , handlePosixSignal
  -- * Resource management
  , rebracket
  , rebracket_
  -- * Deprecated
  , logPrintFDebug
  , liftReader
  , liftActionTaker
  , (??)
  , runCommandFromPath
  ) where

import           Conduit
import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Either.Combinators
import           Data.GI.Base.GError
import           Data.GI.Gtk.Threading as Gtk (postGUIASync, postGUISync)
import           Data.GI.Gtk.Threading (postGUIASyncWithPriority)
import           Data.Maybe
import           Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import           Data.Tuple.Sequence
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.GLib.Constants as G
import           Network.HTTP.Simple
import           System.Directory
import           System.Environment.XDG.BaseDir
import           System.Exit (ExitCode (..), exitWith)
import           System.FilePath.Posix
import           System.IO (hIsTerminalDevice, stdout, stderr)
import           System.Log.Logger
import           System.Posix.Signals (Signal, Handler(..), installHandler, sigHUP, sigINT)
import qualified System.Process as P
import           Text.Printf
import           UnliftIO.Concurrent (ThreadId, forkIO, threadDelay)
import           UnliftIO.Exception (bracket, catch, catchAny)
import qualified UnliftIO.MVar as MV


taffyStateDir :: IO FilePath
taffyStateDir = getUserDataDir "taffybar"

{-# DEPRECATED liftReader "Use Control.Monad.Trans.Reader.mapReaderT instead" #-}
liftReader :: Monad m => (m1 a -> m b) -> ReaderT r m1 a -> ReaderT r m b
liftReader = mapReaderT

logPrintF
  :: (MonadIO m, Show t)
  => String -> Priority -> String -> t -> m ()
logPrintF logPath priority format toPrint =
  liftIO $ logM logPath priority $ printf format $ show toPrint

{-# DEPRECATED logPrintFDebug "Use logPrintF instead" #-}
logPrintFDebug :: (MonadIO m, Show t) => String -> String -> t -> m ()
logPrintFDebug path = logPrintF path DEBUG

infixl 4 ??
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab
{-# INLINE (??) #-}
{-# DEPRECATED (??) "Use @f <*> pure a@ instead" #-}

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond whenTrue whenFalse =
  cond >>= (\bool -> if bool then whenTrue else whenFalse)

forkM :: Monad m => (c -> m a) -> (c -> m b) -> c -> m (a, b)
forkM a b = sequenceT . (a &&& b)

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

truncateString :: Int -> String -> String
truncateString n incoming
  | length incoming <= n = incoming
  | otherwise = take n incoming ++ "…"

truncateText :: Int -> T.Text -> T.Text
truncateText n incoming
  | T.length incoming <= n = incoming
  | otherwise = T.append (T.take n incoming) "…"

-- | Run the provided command with the provided arguments.
--
-- If the command filename does not contain a slash, then the @PATH@
-- environment variable is searched for the executable.
runCommand :: MonadIO m => FilePath -> [String] -> m (Either String String)
runCommand cmd args = liftIO $ do
  (ecode, out, err) <- P.readProcessWithExitCode cmd args ""
  logM "System.Taffybar.Util" INFO $
       printf "Running command %s with args %s" (show cmd) (show args)
  return $ case ecode of
    ExitSuccess -> Right out
    ExitFailure exitCode -> Left $ printf "Exit code %s: %s " (show exitCode) err

{-# DEPRECATED runCommandFromPath "Use runCommand instead" #-}
runCommandFromPath :: MonadIO m => FilePath -> [String] -> m (Either String String)
runCommandFromPath = runCommand

-- | A variant of 'bracket' which allows for reloading.
--
-- The first parameter is an allocation function which returns a newly
-- created value of type @r@, paired with an @IO@ action which will
-- destroy that value.
--
-- The second parameter is the action to run. It is passed a "reload"
-- function which will run the allocation function and return the
-- newly created value.
--
-- Initially, there is no value. Reloading will cause the previous
-- value (if any) to be destroyed. When the action completes, the
-- current value (if any) will be destroyed.
rebracket :: IO (IO (), r) -> (IO r -> IO a) -> IO a
rebracket alloc action = bracket setup teardown (action . reload)
  where
    cleanup = fst
    resource = snd
    setup = MV.newMVar Nothing
    teardown = maybeTeardown <=< MV.takeMVar
    maybeTeardown = maybe (pure ()) cleanup
    reload var = MV.modifyMVar var $ \stale -> do
      maybeTeardown stale
      fresh <- alloc
      pure (Just fresh, resource fresh)

-- | A variant of 'rebracket' where the resource value isn't needed.
--
-- And because the resource value isn't needed, this variant will
-- automatically allocate the resource before running the enclosed
-- action.
rebracket_ :: IO (IO ()) -> (IO () -> IO a) -> IO a
rebracket_ alloc action = rebracket ((, ()) <$> alloc) $
  \reload -> reload >> action reload

-- | Execute the provided IO action at the provided interval.
foreverWithDelay :: (MonadUnliftIO m, RealFrac d) => d -> m () -> m ThreadId
foreverWithDelay delay action =
  foreverWithVariableDelay $ safeAction >> return delay
  where safeAction =
          catchAny action $ \e ->
            logPrintF "System.Taffybar.Util" WARNING "Error in foreverWithDelay %s" e

-- | Execute the provided IO action, and use the value it returns to decide how
-- long to wait until executing it again. The value returned by the action is
-- interpreted as a number of seconds.
foreverWithVariableDelay :: (MonadUnliftIO m, RealFrac d) => m d -> m ThreadId
foreverWithVariableDelay action = forkIO $ action >>= delayThenAction
  where delayThenAction delay =
          threadDelay (floor $ delay * 1000000) >> action >>= delayThenAction

liftActionTaker
  :: (Monad m)
  => ((a -> m a) -> m b) -> (a -> ReaderT c m a) -> ReaderT c m b
liftActionTaker actionTaker action = do
  ctx <- ask
  lift $ actionTaker $ flip runReaderT ctx . action

maybeTCombine
  :: Monad m
  => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
maybeTCombine a b = runMaybeT $ MaybeT a <|> MaybeT b

infixl 3 <||>
(<||>) ::
  Monad m =>
  (t -> m (Maybe a)) -> (t -> m (Maybe a)) -> t -> m (Maybe a)
a <||> b = combineOptions
  where combineOptions v = maybeTCombine (a v) (b v)

infixl 3 <|||>
(<|||>)
  :: Monad m
  => (t -> t1 -> m (Maybe a))
  -> (t -> t1 -> m (Maybe a))
  -> t
  -> t1
  -> m (Maybe a)
a <|||> b = combineOptions
  where combineOptions v v1 = maybeTCombine (a v v1) (b v v1)

catchGErrorsAsLeft :: IO a -> IO (Either GError a)
catchGErrorsAsLeft action = catch (Right <$> action) (return . Left)

catchGErrorsAsNothing :: IO a -> IO (Maybe a)
catchGErrorsAsNothing = fmap rightToMaybe . catchGErrorsAsLeft

safePixbufNewFromFile :: FilePath -> IO (Maybe Gdk.Pixbuf)
safePixbufNewFromFile =
  handleResult . catchGErrorsAsNothing . Gdk.pixbufNewFromFile
  where
#if MIN_VERSION_gi_gdkpixbuf(2,0,26)
    handleResult = fmap join
#else
    handleResult = id
#endif

getPixbufFromFilePath :: FilePath -> IO (Maybe Gdk.Pixbuf)
getPixbufFromFilePath filepath = do
  result <- safePixbufNewFromFile filepath
  when (isNothing result) $
       logM "System.Taffybar.WindowIcon" WARNING $
            printf "Failed to load icon from filepath %s" filepath
  return result

downloadURIToPath :: Request -> FilePath -> IO ()
downloadURIToPath uri filepath =
  createDirectoryIfMissing True directory >>
  runConduitRes (httpSource uri getResponseBody .| sinkFile filepath)
  where (directory, _) = splitFileName filepath

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = return False
anyM p (x:xs) = do
  q <- p x
  if q
  then return True
  else anyM p xs

-- | Installs a useful posix signal handler for 'sigINT' (i.e. Ctrl-C)
-- for cases when the 'Control.Exception.UserInterrupt' exception gets
-- swallowed within a main loop, preventing the program from exiting.
--
-- The given callback should be a command which causes the main loop
-- action to exit. For example:
--
-- > Gtk.main `onSigINT` Gtk.mainQuit
--
-- If the signal handler was invoked, the program will exit with
-- status 130 after the main loop action returns.
onSigINT
  :: IO a -- ^ The main loop 'IO' action
  -> IO () -- ^ Callback for @SIGINT@
  -> IO a
onSigINT action callback = do
  exitStatus <- newIORef Nothing

  let intHandler = do
        writeIORef exitStatus (Just (ExitFailure 130))
        callback

  withSigHandlerBase sigINT (CatchOnce intHandler) $ do
    res <- action
    readIORef exitStatus >>= mapM_ exitWith
    pure res

-- | Installs the given function as a handler for @SIGHUP@, but only
-- if this process is not running in a terminal (i.e. runnning as a
-- daemon).
--
-- If not running as a daemon, then no handler is installed by
-- 'maybeHandleSigHUP'. The default handler for 'sigHUP' exits the
-- program, which is the correct thing to do.
maybeHandleSigHUP :: IO () -> IO a -> IO a
maybeHandleSigHUP callback action =
  ifM (anyM hIsTerminalDevice [stdout, stderr])
    action
    (handlePosixSignal sigHUP callback action)

-- | Install a handler for the given POSIX 'Signal' while the given
-- @IO@ action is running, then restore the original handler.
--
-- This function is for handling non-critical signals.
--
-- The given callback function won't be run immediately within the
-- @sigaction@ handler, but will instead be posted to the GLib main
-- loop.
handlePosixSignal :: Signal -> IO () -> IO a -> IO a
handlePosixSignal sig cb = withSigHandlerBase sig (Catch handler)
  where
    handler = postGUIASyncWithPriority G.PRIORITY_HIGH_IDLE cb

-- | Install a handler for the given signal, run an 'IO' action, then
-- restore the original handler.
withSigHandlerBase :: Signal -> Handler -> IO a -> IO a
withSigHandlerBase sig h = bracket (install h) install . const
  where
    install handler = installHandler sig handler Nothing
