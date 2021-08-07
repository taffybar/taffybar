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

module System.Taffybar.Util where

import           Conduit
import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Concurrent
import           Control.Exception.Base
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Either.Combinators
import           Data.GI.Base.GError
import qualified Data.GI.Gtk.Threading as Gtk
import           Data.Maybe
import qualified Data.Text as T
import           Data.Tuple.Sequence
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import           Network.HTTP.Simple
import           System.Directory
import           System.Environment.XDG.BaseDir
import           System.Exit (ExitCode (..))
import           System.FilePath.Posix
import           System.Log.Logger
import qualified System.Process as P
import           Text.Printf


taffyStateDir :: IO FilePath
taffyStateDir = getUserDataDir "taffybar"

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
truncateString n incoming
  | length incoming <= n = incoming
  | otherwise = take n incoming ++ "…"

truncateText :: Int -> T.Text -> T.Text
truncateText n incoming
  | T.length incoming <= n = incoming
  | otherwise = T.append (T.take n incoming) "…"

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
foreverWithDelay :: (MonadIO m, RealFrac d) => d -> IO a -> m ThreadId
foreverWithDelay delay action =
  foreverWithVariableDelay $ action >> return delay

-- | Execute the provided IO action, and use the value it returns to decide how
-- long to wait until executing it again. The value returned by the action is
-- interpreted as a number of seconds.
foreverWithVariableDelay :: (MonadIO m, RealFrac d) => IO d -> m ThreadId
foreverWithVariableDelay action = liftIO $ forkIO $ action >>= delayThenAction
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
  (runConduitRes $ httpSource uri getResponseBody .| sinkFile filepath)
  where (directory, _) = splitFileName filepath

postGUIASync :: IO () -> IO ()
postGUIASync = Gtk.postGUIASync

postGUISync :: IO () -> IO ()
postGUISync = Gtk.postGUISync
