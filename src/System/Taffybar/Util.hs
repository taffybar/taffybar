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

module System.Taffybar.Util where

import           Control.Arrow ((&&&))
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Tuple.Sequence
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import           System.Exit (ExitCode (..))
import           System.Log.Logger
import qualified System.Process as P
import           Text.Printf

liftReader ::
  Monad m => (m1 a -> m b) -> ReaderT r m1 a -> ReaderT r m b
liftReader modifier action =
  ask >>= lift . modifier . runReaderT action

logPrintF
  :: (MonadIO m, Show t)
  => String -> Priority -> String -> t -> m ()
logPrintF logPath priority format toPrint =
  liftIO $ logM logPath priority $ printf format $ show toPrint

logPrintFDebug :: (MonadIO m, Show t) => String -> String -> t -> m ()
logPrintFDebug path = logPrintF path DEBUG

infixl 4 ??
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab
{-# INLINE (??) #-}

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond whenTrue whenFalse =
  cond >>= (\bool -> if bool then whenTrue else whenFalse)

forkM :: Monad m => (c -> m a) -> (c -> m b) -> c -> m (a, b)
forkM a b = sequenceT . (a &&& b)

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

truncateString :: Int -> String -> String
truncateString n xs
  | length xs <= n = xs
  | otherwise      = take n xs ++ "…"

runOnUIThread :: MonadIO m => IO a -> m ()
runOnUIThread action =
  void $ Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $
       action >> return False

runCommandFromPath :: MonadIO m => [String] -> m (Either String String)
runCommandFromPath = runCommand "/usr/bin/env"

-- | Run the provided command with the provided arguments.
runCommand :: MonadIO m => FilePath -> [String] -> m (Either String String)
runCommand cmd args = liftIO $ do
  (ecode, stdout, stderr) <- P.readProcessWithExitCode cmd args ""
  logM "System.Taffybar.Util" INFO $
       printf "Running command %s with args %s" (show cmd) (show args)
  return $ case ecode of
    ExitSuccess -> Right stdout
    ExitFailure exitCode -> Left $ printf "Exit code %s: %s " (show exitCode) stderr

-- | Execute the provided IO action at the provided interval.
foreverWithDelay :: RealFrac a1 => a1 -> IO a -> IO ThreadId
foreverWithDelay delay action =
  forkIO $ forever $ action >> threadDelay (floor $ delay * 1000000)

liftActionTaker
  :: (Monad m)
  => ((a -> m a) -> m b) -> (a -> ReaderT c m a) -> ReaderT c m b
liftActionTaker actionTaker action = do
  ctx <- ask
  lift $ actionTaker $ flip runReaderT ctx . action

xdgOpen :: MonadIO m => [String] -> m (Either String String)
xdgOpen args = runCommandFromPath ("xdg-open":args)

openURL :: MonadIO m => String -> m (Either String String)
openURL = xdgOpen . return
