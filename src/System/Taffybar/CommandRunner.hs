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
import           System.Taffybar.Widgets.PollingLabel

import           System.Process                       hiding (readProcess,
                                                       runCommand)

import           Prelude                              hiding (mapM)

import           Control.Concurrent
import qualified Control.Exception                    as C
import           Control.Monad
import           System.Exit                          (ExitCode (..))
import           System.IO

-- | Creates a new command runner widget. This is a 'PollingLabel' fed by
-- regular calls to command given by argument. The results of calling this function
-- are displayed as string.
commandRunnerNew :: Double   -- ^ Polling period (in seconds).
                 -> String   -- ^ Command to execute. Should be in $PATH or an absolute path
                 -> [String] -- Command argyment. May be []
                 -> String   -- If command failes this would be displayed.
                 -> IO Gtk.Widget
commandRunnerNew interval cmd args defaultOutput = do
    label  <- pollingLabelNew "" interval $ runCommand cmd args defaultOutput
    Gtk.widgetShowAll label
    return $ Gtk.toWidget label

runCommand :: FilePath -> [String] -> String -> IO String
runCommand cmd args defaultOutput = do
    result <- readProcess cmd args []
    case result of
         Nothing -> return defaultOutput
         Just a  -> return a

readProcess :: FilePath -> [String] -> String -> IO (Maybe String) -- had to modify function from library
readProcess cmd args input = do
    (Just inh, Just outh, _, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = Inherit }
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    forkIO $ C.evaluate (length output) >> putMVar outMVar ()
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh
    takeMVar outMVar
    hClose outh
    ex <- waitForProcess pid
    case ex of
        ExitSuccess   -> return $ Just output
        ExitFailure _ -> return Nothing
