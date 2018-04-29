--------------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.CommandRunner
-- Copyright   : (c) Arseniy Seroka
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Arseniy Seroka <ars.seroka@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple function which runs user defined command and
-- returns it's output in PollingLabel widget
--------------------------------------------------------------------------------

module System.Taffybar.Widget.CommandRunner ( commandRunnerNew ) where

import           Control.Monad.Trans
import qualified Graphics.UI.Gtk as Gtk
import qualified System.IO as IO
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.PollingLabel
import           System.Taffybar.Widget.Util

-- | Creates a new command runner widget. This is a 'PollingLabel' fed by
-- regular calls to command given by argument. The results of calling this
-- function are displayed as string.
commandRunnerNew
  :: MonadIO m
  => Double -- ^ Polling period (in seconds).
  -> String -- ^ Command to execute. Should be in $PATH or an absolute path
  -> [String] -- ^ Command argument. May be @[]@
  -> String -- ^ If command fails this will be displayed.
  -> String -- ^ Output color
  -> m Gtk.Widget
commandRunnerNew interval cmd args defaultOutput color = liftIO $ do
    label  <- pollingLabelNew "" interval $
              runCommandWithDefault cmd args defaultOutput color
    Gtk.widgetShowAll label
    return $ Gtk.toWidget label

runCommandWithDefault :: FilePath -> [String] -> [Char] -> String -> IO String
runCommandWithDefault cmd args def color =
  colorize color "" <$>
  (runCommand cmd args >>= either printError return)
  where printError err = IO.hPutStrLn IO.stderr err >> return def
