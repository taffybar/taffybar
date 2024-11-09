{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

module System.Taffybar.Test.UtilSpec
  ( spec
  -- * Mock commands
  , withMockCommand
  , writeScript
  -- * Environment setup
  , withEnv
  , withSetEnv
  , prependPath
  -- ** Running subprocesses
  , withService
  , setStdoutCond
  , setStderrCond
  , setServiceDefaults
  , makeServiceDefaults
  -- * Concurrency
  , listLiveThreads
  , diffLiveThreads
  -- * OS Resources
  , listFds
  -- * Other test helpers
  , tryIOMaybe
  , laxTimeout
  , laxTimeout'
  , DodgyEq(..)
  -- ** Logging for tests
  , logSetup
  , specLogSetup
  , specLogSetupPrio
  , specLog
  , specLogAt
  , getSpecLogPriority
  , Priority(..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard, join, void, (<=<))
import Control.Monad.IO.Unlift
import Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as B8
import Data.Either.Extra (eitherToMaybe, isLeft)
import Data.Function (on, (&))
import Data.List (deleteFirstsBy, uncons)
import Data.Maybe (catMaybes, fromMaybe)
#if MIN_VERSION_base(4,18,0)
import GHC.Conc.Sync (ThreadId(..), ThreadStatus(..), listThreads, threadStatus, threadLabel)
#else
import GHC.Conc.Sync (ThreadId(..), ThreadStatus(..))
#endif
import System.Exit (ExitCode(..))
import System.FilePath (isRelative, takeFileName, (</>))
import System.IO (Handle, BufferMode(..), hSetBuffering, stderr, hClose)
import System.Log.Logger (Priority(..), updateGlobalLogger, setLevel, logM, getLevel, getLogger, removeHandler, setHandlers)
import System.Log.Handler.Simple (GenericHandler(..))
import System.Process.Typed (readProcess, proc, ProcessConfig, Process, withProcessTerm, waitExitCode, ExitCodeException (..), setStdin, nullStream, setStdout, setStderr, StreamSpec, setCloseFds, inherit, createPipe, getStdin)
import System.Posix.Files (readSymbolicLink)
import Test.Hspec
import Text.Printf (printf)
import Text.Read (readMaybe)
import UnliftIO.Async (race)
import UnliftIO.Concurrent (forkFinally, threadDelay)
import UnliftIO.Directory (Permissions (..), findExecutable, getPermissions, setPermissions, listDirectory)
import UnliftIO.Environment (lookupEnv, setEnv, unsetEnv)
import UnliftIO.Exception (bracket, evaluateDeep, throwIO, throwString, tryIO, StringException (..), try)
import qualified UnliftIO.MVar as MV
import UnliftIO.Temporary (withSystemTempDirectory)
import UnliftIO.Timeout (timeout)

import System.Taffybar.LogFormatter (taffyLogHandler)

-- | Run the given 'IO' action with the @PATH@ environment variable
-- set up so that executing the given command name will run a
-- script.
withMockCommand
  :: FilePath -- ^ Name of command - should not contain slashes
  -> String -- ^ Contents of script
  -> IO a -- ^ Action to run with command available in search path
  -> IO a
withMockCommand name content action =
  withSystemTempDirectory "specutil" $ \dir -> do
    writeScript (dir </> takeFileName name) content
    withEnv [("PATH", prependPath dir)] action

-- | Write a text file, make it executable.
-- It ought to have a shebang line.
writeScript :: FilePath -> String -> IO ()
writeScript scriptFile content = do
  content' <- patchShebangs content
  writeFile scriptFile content'
  p <- getPermissions scriptFile
  setPermissions scriptFile (p { executable = True })

-- | Given the text of a shell script, this replaces any relative path
-- in the shebang with an absolute path, according to the current
-- environment's @PATH@ variable.
--
-- The only reason this exists is so that we can generate shell
-- scripts containing @#!/usr/bin/env bash@ and then be able to
-- execute them within a Nix build sandbox (which does not allow
-- @/usr/bin/env@).
patchShebangs :: String -> IO String
patchShebangs = patchShebangs' findExe
  where
    findExe = fmap join . traverse findExecutable . takeRelativeFileName

    takeRelativeFileName :: FilePath -> Maybe FilePath
    takeRelativeFileName fp = guard (isRelative fp) >> pure (takeFileName fp)

patchShebangs' :: Applicative m => (FilePath -> m (Maybe FilePath)) -> String -> m String
patchShebangs' replaceExe script = case parseInterpreter script of
  Just (interpreter, rest) -> do
    let unparse exe = "#! " ++ exe ++ rest
    maybe script unparse <$> replaceExe interpreter
  Nothing -> pure script

parseInterpreter :: String -> Maybe (String, String)
parseInterpreter (lines -> content) = do
  (header, rest) <- uncons content
  (interpreter, args) <- parseShebang header
  pure (interpreter, unlines (args:rest))

  where
    parseShebang :: String -> Maybe (String, String)
    parseShebang ('#':'!':(findInterpreter -> shebang)) =
      let catArgs args = unwords ("":args)
      in second catArgs <$> shebang
    parseShebang _ = Nothing

    findInterpreter = uncons . dropWhile ((== "env") . takeFileName) . words

-- | Run an 'IO' action with the given environment variables set up
-- according to their current value. 'Nothing' denotes an unset
-- environment variable. After the 'IO' action completes, environment
-- variables are restored to their previous state.
withEnv :: [(String, Maybe String -> Maybe String)] -> IO a -> IO a
withEnv mods = bracket setup teardown . const
  where
    setup = mapM (uncurry changeEnv) mods
    teardown = mapM (uncurry putEnv) . reverse

    changeEnv name f = do
      old <- lookupEnv name
      putEnv name (f old)
      pure (name, old)

    putEnv :: String -> Maybe String -> IO ()
    putEnv name = maybe (unsetEnv name) (setEnv name)

withSetEnv :: [(String, String)] -> IO a -> IO a
withSetEnv = withEnv . map (second (const . Just))

-- | Use this as a modifier function argument of 'withEnv' to ensure
-- that the given directory is prepended to a search path variable.
prependPath :: FilePath -> Maybe String -> Maybe String
prependPath p = Just . (++ ":/usr/bin") . (p ++) . maybe "" (":" ++)

listFds :: MonadIO m => m [(Int, FilePath)]
listFds = catMaybes <$> (listDirectory fdPath >>= mapM readEntry)
  where
    fdPath = "/proc/self/fd"

    readEntry :: MonadIO m => FilePath -> m (Maybe (Int, FilePath))
    readEntry fd = do
      t <- liftIO $ tryIOMaybe $ readSymbolicLink (fdPath </> fd)
      pure $ (,) <$> readMaybe fd <*> t

listLiveThreads :: IO [(ThreadId, (String, Maybe ThreadStatus))]
#if MIN_VERSION_base(4,18,0)
listLiveThreads = do
  threadIds <- listThreads
  labels <- mapM (fmap (fromMaybe "" . join) . tryIOMaybe . threadLabel) threadIds
  statuses <- mapM (tryIOMaybe . threadStatus) threadIds
  let isAlive s = s /= ThreadFinished && s /= ThreadDied
  pure $ filter (maybe True isAlive . snd . snd) $ zip threadIds (zip labels statuses)
#else
listLiveThreads = pure []
#endif

diffLiveThreads :: Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]
diffLiveThreads = deleteFirstsBy ((==) `on` fst)

tryIOMaybe :: MonadUnliftIO m => m a -> m (Maybe a)
tryIOMaybe = fmap eitherToMaybe . tryIO

laxTimeout' :: (HasCallStack, MonadUnliftIO m) => Int -> m a -> m a
laxTimeout' n action = laxTimeout n action >>= \case
  Just a -> pure a
  Nothing -> expectationFailure' $ printf "Timed out after %dusec" n

expectationFailure' :: (HasCallStack, MonadIO m) => String -> m a
expectationFailure' msg = liftIO (expectationFailure msg) >> throwString msg

laxTimeout :: (HasCallStack, MonadUnliftIO m) => Int -> m a -> m (Maybe a)
laxTimeout n action = do
  result <- MV.newEmptyMVar
  void $ forkFinally (timeout n action) (MV.putMVar result)
  join <$> timeout n (MV.takeMVar result >>= either throwIO pure)

-- | A wrapper to provide 'Eq' for types which only have 'Show'.
newtype DodgyEq a = DodgyEq { unDodgyEq :: a }
  deriving Show via (DodgyEq a)

instance Eq (DodgyEq a) where
  a == b = show a == show b

------------------------------------------------------------------------

-- | Logger name for messages originating from specs.
specLoggerName :: String
specLoggerName = "Test"

-- | Log a test message.
specLog :: MonadIO m => String -> m ()
specLog = specLogAt INFO

-- | Log a test message at the given level.
specLogAt :: MonadIO m => Priority -> String -> m ()
specLogAt level = liftIO . logM specLoggerName level

-- | Setup logging before running the specs.
logSetup :: HasCallStack => SpecWith a -> SpecWith a
logSetup = beforeAll_ specLogSetup

-- | Get log levels from environment variables and set up formatters.
specLogSetup :: IO ()
specLogSetup = specLogSetupPrio WARNING

-- | Like 'specLogSetup', but with a default minimum priority.
specLogSetupPrio :: Priority -> IO ()
specLogSetupPrio defaultPriority = do
  updateGlobalLogger "" removeHandler
  hSetBuffering stderr LineBuffering
  setup "System.Taffybar" "TAFFYBAR_VERBOSE" taffyLogHandler
  setup specLoggerName "TAFFYBAR_TEST_VERBOSE" (pure specLogHandler)
  where
    setup loggerName envVar getHandler = do
      p <- fromMaybe defaultPriority <$> getEnvPriority envVar
      h <- getHandler
      updateGlobalLogger loggerName (setLevel p . setHandlers [h])

-- | A plain looking log handler, to contrast with 'taffyLogFormatter'.
specLogHandler :: GenericHandler Handle
specLogHandler = GenericHandler
  { priority = DEBUG
  , formatter =  \_ (level, msg) _name -> return (show level ++ ": " ++ msg)
  , privData = stderr
  , writeFunc = \h -> B8.hPutStrLn h . B8.pack <=< evaluateDeep
  , closeFunc = \_ -> return ()
  }

-- | Find out the configured log level for specs.
getSpecLogPriority :: MonadIO m => m Priority
getSpecLogPriority = fromMaybe WARNING . getLevel <$> liftIO (getLogger specLoggerName)

-- | Converts an environment variable value to a 'Priority'.
-- Numeric or textual levels are supported.
getEnvPriority :: String -> IO (Maybe Priority)
getEnvPriority = fmap (>>= toPriority) . lookupEnv
  where
    toPriority s = readMaybe s <|> fmap fromInt (readMaybe s)

    fromInt :: Int -> Priority
    fromInt n | n >= 2 = DEBUG
              | n <= 0 = WARNING
              | otherwise = INFO

-- | Like 'withProcessTerm_', except that if the process exits -- for
-- whatever reason -- before the action completes, then it's an
-- error. It will immediately cancel the action and throw an
-- 'ExitCodeException'.
withService :: MonadUnliftIO m => ProcessConfig stdin stdout stderr -> (Process stdin stdout stderr -> m a) -> m a
withService cfg action = withProcessTerm cfg $ \p -> do
  either throwEarlyExitException pure =<< race (waitExitCode p) (action p)
  where
    throwEarlyExitException c = throwIO $ ExitCodeException c cfg' "" ""
    cfg' = cfg & setStdin nullStream & setStdout nullStream & setStderr nullStream

streamSpecCond :: Priority -> Priority -> StreamSpec any ()
streamSpecCond level verbosity = if level >= verbosity then inherit else nullStream

setStdoutCond :: Priority -> ProcessConfig i o e -> ProcessConfig i () e
setStdoutCond = setStdout . streamSpecCond DEBUG

setStderrCond :: Priority -> ProcessConfig i o e -> ProcessConfig i o ()
setStderrCond = setStderr . streamSpecCond INFO

makeServiceDefaults :: FilePath -> [String] -> IO (ProcessConfig () () ())
makeServiceDefaults prog args =
  ($ proc prog args) . setServiceDefaults <$> getSpecLogPriority

setServiceDefaults :: Priority -> ProcessConfig i o e -> ProcessConfig () () ()
setServiceDefaults logLevel = setCloseFds True
  . setStdin nullStream
  . setStdoutCond logLevel
  . setStderrCond logLevel

------------------------------------------------------------------------

spec :: Spec
spec = do
  it "withMockCommand" $ example $
    withMockCommand "blah" "#!/usr/bin/env bash\necho hello \"$@\"\n" $ do
      (code, out, err) <- readProcess (proc "blah" ["arstd"])
      code `shouldBe` ExitSuccess
      out `shouldBe` "hello arstd\n"
      err `shouldBe` ""

  it "laxTimeout" $ example $ do
    let t = 50_000
    laxTimeout t (threadDelay (t * 2)) `shouldReturn` Nothing

  describe "withService" $ around_ (laxTimeout' 100_000) $ do
    let wait = const $ threadDelay maxBound
    it "normal" $ example $
      withService (proc "sleep" ["60"]) (const $ pure ())
        `shouldReturn` ()
    it "exc" $ example $
      withService (proc "sleep" ["60"]) (const $ throwString "hello")
        `shouldThrow` \(StringException msg _) -> msg == "hello"
    it "early exit" $ example $
      withService "true" wait `shouldThrow`
        \exc -> eceExitCode exc == ExitSuccess
    it "manual exit" $ example $
      withService
        (proc "cat" [] & setStdin createPipe)
        (\p -> hClose (getStdin p) >> wait p)
        `shouldThrow` \exc -> eceExitCode exc == ExitSuccess
    it "failure" $ example $
      withService "false" wait `shouldThrow`
        \exc -> eceExitCode exc /= ExitSuccess
    it "error message" $ example $ do
      res <- try (withService (proc "false" ["arg1", "arg2"]) wait)
      res `shouldSatisfy` isLeft
      either (show . eceProcessConfig) show res
        `shouldBe` "Raw command: false arg1 arg2\n"
