{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.KeyboardState
-- Copyright   : (c) Ivan Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- A widget that displays keyboard lock key states (Caps Lock, Num Lock,
-- Scroll Lock). States are read from sysfs LED brightness files.
--
--------------------------------------------------------------------------------

module System.Taffybar.Widget.KeyboardState
  ( -- * Widget Constructors
    keyboardStateNew
  , keyboardStateNewWithConfig
    -- * Configuration
  , KeyboardStateConfig(..)
  , defaultKeyboardStateConfig
    -- * Format Functions
  , formatKeyboardState
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import System.Taffybar.Information.KeyboardState
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNew)
import System.Taffybar.Widget.Util (widgetSetClassGI)

-- | Configuration for the keyboard state widget.
data KeyboardStateConfig = KeyboardStateConfig
  { kscCapsLockOnText :: T.Text
    -- ^ Text to display when Caps Lock is on. Default: "[CAPS]"
  , kscCapsLockOffText :: T.Text
    -- ^ Text to display when Caps Lock is off. Default: ""
  , kscNumLockOnText :: T.Text
    -- ^ Text to display when Num Lock is on. Default: "[NUM]"
  , kscNumLockOffText :: T.Text
    -- ^ Text to display when Num Lock is off. Default: ""
  , kscScrollLockOnText :: T.Text
    -- ^ Text to display when Scroll Lock is on. Default: "[SCROLL]"
  , kscScrollLockOffText :: T.Text
    -- ^ Text to display when Scroll Lock is off. Default: ""
  , kscShowCapsLock :: Bool
    -- ^ Whether to show Caps Lock state. Default: True
  , kscShowNumLock :: Bool
    -- ^ Whether to show Num Lock state. Default: False
  , kscShowScrollLock :: Bool
    -- ^ Whether to show Scroll Lock state. Default: False
  , kscSeparator :: T.Text
    -- ^ Separator between lock indicators. Default: " "
  , kscPollingInterval :: Double
    -- ^ Polling interval in seconds. Default: 0.5
  } deriving (Eq, Show)

-- | Default configuration for the keyboard state widget.
-- Shows only Caps Lock by default, polling every 0.5 seconds.
defaultKeyboardStateConfig :: KeyboardStateConfig
defaultKeyboardStateConfig = KeyboardStateConfig
  { kscCapsLockOnText = "[CAPS]"
  , kscCapsLockOffText = ""
  , kscNumLockOnText = "[NUM]"
  , kscNumLockOffText = ""
  , kscScrollLockOnText = "[SCROLL]"
  , kscScrollLockOffText = ""
  , kscShowCapsLock = True
  , kscShowNumLock = False
  , kscShowScrollLock = False
  , kscSeparator = " "
  , kscPollingInterval = 0.5
  }

-- | Format the keyboard state according to the configuration.
formatKeyboardState :: KeyboardStateConfig -> KeyboardState -> T.Text
formatKeyboardState cfg state =
  let parts = filter (not . T.null)
        [ if kscShowCapsLock cfg
          then if capsLock state
               then kscCapsLockOnText cfg
               else kscCapsLockOffText cfg
          else ""
        , if kscShowNumLock cfg
          then if numLock state
               then kscNumLockOnText cfg
               else kscNumLockOffText cfg
          else ""
        , if kscShowScrollLock cfg
          then if scrollLock state
               then kscScrollLockOnText cfg
               else kscScrollLockOffText cfg
          else ""
        ]
  in T.intercalate (kscSeparator cfg) parts

-- | Create a keyboard state widget with default configuration.
-- Shows Caps Lock status, polling every 0.5 seconds.
keyboardStateNew :: MonadIO m => m Gtk.Widget
keyboardStateNew = keyboardStateNewWithConfig defaultKeyboardStateConfig

-- | Create a keyboard state widget with custom configuration.
-- Uses PollingLabel to periodically update the display.
keyboardStateNewWithConfig :: MonadIO m => KeyboardStateConfig -> m Gtk.Widget
keyboardStateNewWithConfig cfg = liftIO $ do
  widget <- pollingLabelNew (kscPollingInterval cfg) (getFormattedState cfg)
  _ <- widgetSetClassGI widget "keyboard-state"
  return widget

-- | Get the formatted keyboard state text.
getFormattedState :: KeyboardStateConfig -> IO T.Text
getFormattedState cfg = do
  state <- getKeyboardState
  return $ formatKeyboardState cfg state
