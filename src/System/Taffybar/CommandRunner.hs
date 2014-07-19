--------------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.CommandRunner
-- Copyright   : (c) Arseniy Seroka
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Arseniy Seroka <ars.seroka@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple function which runs user defined command and
-- returns it's output in PollingLabel widget
--
--------------------------------------------------------------------------------

module System.Taffybar.CommandRunner ( commandRunnerNew ) where

import qualified Graphics.UI.Gtk                      as Gtk
import           System.Taffybar.Pager                (colorize)
import           System.Taffybar.Widgets.PollingLabel

import           Control.Monad
import           System.Exit                          (ExitCode (..))
import qualified System.IO as IO
import qualified System.Process as P

-- | Creates a new command runner widget. This is a 'PollingLabel' fed by
-- regular calls to command given by argument. The results of calling this function
-- are displayed as string.
commandRunnerNew :: Double   -- ^ Polling period (in seconds).
                 -> String   -- ^ Command to execute. Should be in $PATH or an absolute path
                 -> [String] -- ^ Command argument. May be []
                 -> String   -- ^ If command fails this will be displayed.
                 -> String   -- ^ Output color
                 -> IO Gtk.Widget
commandRunnerNew interval cmd args defaultOutput color = do
    label  <- pollingLabelNew "" interval $ runCommand cmd args defaultOutput color
    Gtk.widgetShowAll label
    return $ Gtk.toWidget label

runCommand :: FilePath -> [String] -> String -> String -> IO String
runCommand cmd args defaultOutput color = do
  (ecode, stdout, stderr) <- P.readProcessWithExitCode cmd args ""
  unless (null stderr) $ do
    IO.hPutStrLn IO.stderr stderr
  return $ colorize color "" $ case ecode of
    ExitSuccess -> stdout
    ExitFailure _ -> defaultOutput
--     result <- readProcess cmd args []
--     return $ colorize color "" $ fromMaybe defaultOutput result
--     -- return $ colorize color "" $ case result of
--     --      Nothing -> defaultOutput
--     --      Just a  -> a

-- readProcess :: FilePath -> [String] -> String -> IO (Maybe String) -- had to modify function from library
-- readProcess cmd args input = do
--     (Just inh, Just outh, _, pid) <-
--       P.createProcess (P.proc cmd args){ P.std_in  = P.CreatePipe,
--                                          P.std_out = P.CreatePipe,
--                                          P.std_err = P.Inherit
--                                        }
--     output  <- IO.hGetContents outh
--     outMVar <- C.newEmptyMVar
--     C.forkIO $ E.evaluate (length output) >> C.putMVar outMVar ()
--     when (not (null input)) $ do
--       IO.hPutStr inh input
--       IO.hFlush inh
--     IO.hClose inh
--     C.takeMVar outMVar
--     IO.hClose outh
--     ex <- P.waitForProcess pid
--     case ex of
--       ExitSuccess   -> return $ Just output
--       ExitFailure _ -> return Nothing
