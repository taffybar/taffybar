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
                 -> [String] -- ^ Command argument. May be @[]@
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
