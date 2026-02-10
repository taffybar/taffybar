-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.ScreenLock
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides screen lock functionality as a thin wrapper around
-- "System.Taffybar.Information.Inhibitor". It re-exports the inhibitor
-- management functions and adds a 'lockScreen' action that spawns hyprlock.
-----------------------------------------------------------------------------
module System.Taffybar.Information.ScreenLock
  ( -- * Re-exports from Inhibitor
    getInhibitorChan
  , getInhibitorState
  , toggleInhibitor
    -- * Screen Lock
  , lockScreen
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class
import System.Process (spawnCommand)

import System.Taffybar.Information.Inhibitor
  ( getInhibitorChan
  , getInhibitorState
  , toggleInhibitor
  )

-- | Lock the screen by spawning hyprlock. Output is redirected to
-- @\/dev\/null@ to avoid flooding taffybar's log with hyprlock's
-- verbose Wayland messages.
lockScreen :: MonadIO m => m ()
lockScreen = liftIO $ void $ spawnCommand "hyprlock >/dev/null 2>&1"
