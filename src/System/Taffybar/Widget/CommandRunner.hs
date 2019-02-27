{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Monad.IO.Class
import qualified GI.Gtk
import           System.Log.Logger
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.PollingLabel
import           Text.Printf
import qualified Data.Text as T

-- | Creates a new command runner widget. This is a 'PollingLabel' fed by
-- regular calls to command given by argument. The results of calling this
-- function are displayed as string.
commandRunnerNew
  :: MonadIO m
  => Double -- ^ Polling period (in seconds).
  -> String -- ^ Command to execute. Should be in $PATH or an absolute path
  -> [String] -- ^ Command argument. May be @[]@
  -> T.Text -- ^ If command fails this will be displayed.
  -> m GI.Gtk.Widget
commandRunnerNew interval cmd args defaultOutput =
  pollingLabelNew interval $ runCommandWithDefault cmd args defaultOutput

runCommandWithDefault :: FilePath -> [String] -> T.Text -> IO T.Text
runCommandWithDefault cmd args def =
  T.filter (/= '\n') <$> (runCommand cmd args >>= either logError (return . T.pack))
  where logError err =
          logM "System.Taffybar.Widget.CommandRunner" ERROR
               (printf "Got error in CommandRunner %s" err) >> return def
