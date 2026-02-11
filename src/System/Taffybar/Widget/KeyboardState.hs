{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

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
module System.Taffybar.Widget.KeyboardState
  ( -- * Widget Constructors
    keyboardStateNew,
    keyboardStateNewWithConfig,
    keyboardStateLabelNew,
    keyboardStateLabelNewWithConfig,
    keyboardStateIconNew,
    keyboardStateIconNewWithConfig,

    -- * Configuration
    KeyboardStateConfig (..),
    defaultKeyboardStateConfig,

    -- * Format Functions
    formatKeyboardState,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import System.Taffybar.Information.KeyboardState
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNew)
import System.Taffybar.Widget.Util (buildIconLabelBox, widgetSetClassGI)

-- | Configuration for the keyboard state widget.
data KeyboardStateConfig = KeyboardStateConfig
  { -- | Text to display when Caps Lock is on. Default: "[CAPS]"
    kscCapsLockOnText :: T.Text,
    -- | Text to display when Caps Lock is off. Default: ""
    kscCapsLockOffText :: T.Text,
    -- | Text to display when Num Lock is on. Default: "[NUM]"
    kscNumLockOnText :: T.Text,
    -- | Text to display when Num Lock is off. Default: ""
    kscNumLockOffText :: T.Text,
    -- | Text to display when Scroll Lock is on. Default: "[SCROLL]"
    kscScrollLockOnText :: T.Text,
    -- | Text to display when Scroll Lock is off. Default: ""
    kscScrollLockOffText :: T.Text,
    -- | Whether to show Caps Lock state. Default: True
    kscShowCapsLock :: Bool,
    -- | Whether to show Num Lock state. Default: False
    kscShowNumLock :: Bool,
    -- | Whether to show Scroll Lock state. Default: False
    kscShowScrollLock :: Bool,
    -- | Separator between lock indicators. Default: " "
    kscSeparator :: T.Text,
    -- | Polling interval in seconds. Default: 0.5
    kscPollingInterval :: Double,
    -- | Icon text for the icon widget variant. Default: keyboard icon (nf-md-keyboard)
    kscIcon :: T.Text
  }
  deriving (Eq, Show)

-- | Default configuration for the keyboard state widget.
-- Shows only Caps Lock by default, polling every 0.5 seconds.
defaultKeyboardStateConfig :: KeyboardStateConfig
defaultKeyboardStateConfig =
  KeyboardStateConfig
    { kscCapsLockOnText = "[CAPS]",
      kscCapsLockOffText = "",
      kscNumLockOnText = "[NUM]",
      kscNumLockOffText = "",
      kscScrollLockOnText = "[SCROLL]",
      kscScrollLockOffText = "",
      kscShowCapsLock = True,
      kscShowNumLock = False,
      kscShowScrollLock = False,
      kscSeparator = " ",
      kscPollingInterval = 0.5,
      kscIcon = T.pack "\xF80B"
    }

-- | Format the keyboard state according to the configuration.
formatKeyboardState :: KeyboardStateConfig -> KeyboardState -> T.Text
formatKeyboardState cfg state =
  let parts =
        filter
          (not . T.null)
          [ if kscShowCapsLock cfg
              then
                if capsLock state
                  then kscCapsLockOnText cfg
                  else kscCapsLockOffText cfg
              else "",
            if kscShowNumLock cfg
              then
                if numLock state
                  then kscNumLockOnText cfg
                  else kscNumLockOffText cfg
              else "",
            if kscShowScrollLock cfg
              then
                if scrollLock state
                  then kscScrollLockOnText cfg
                  else kscScrollLockOffText cfg
              else ""
          ]
   in T.intercalate (kscSeparator cfg) parts

-- | Create a keyboard state label widget with default configuration.
-- Shows Caps Lock status, polling every 0.5 seconds.
keyboardStateLabelNew :: (MonadIO m) => m Gtk.Widget
keyboardStateLabelNew = keyboardStateLabelNewWithConfig defaultKeyboardStateConfig

-- | Create a keyboard state label widget with custom configuration.
-- Uses PollingLabel to periodically update the display.
keyboardStateLabelNewWithConfig :: (MonadIO m) => KeyboardStateConfig -> m Gtk.Widget
keyboardStateLabelNewWithConfig cfg = liftIO $ do
  widget <- pollingLabelNew (kscPollingInterval cfg) (getFormattedState cfg)
  _ <- widgetSetClassGI widget "keyboard-state"
  return widget

-- | Create a keyboard state icon widget with default configuration.
keyboardStateIconNew :: (MonadIO m) => m Gtk.Widget
keyboardStateIconNew = keyboardStateIconNewWithConfig defaultKeyboardStateConfig

-- | Create a keyboard state icon widget with custom configuration.
-- Displays a static icon label.
keyboardStateIconNewWithConfig :: (MonadIO m) => KeyboardStateConfig -> m Gtk.Widget
keyboardStateIconNewWithConfig cfg = liftIO $ do
  label <- Gtk.labelNew (Just (kscIcon cfg))
  Gtk.widgetShowAll label
  Gtk.toWidget label

-- | Create a combined keyboard state widget (icon + label) with default
-- configuration.
keyboardStateNew :: (MonadIO m) => m Gtk.Widget
keyboardStateNew = keyboardStateNewWithConfig defaultKeyboardStateConfig

-- | Create a combined keyboard state widget (icon + label) with custom
-- configuration.
keyboardStateNewWithConfig :: (MonadIO m) => KeyboardStateConfig -> m Gtk.Widget
keyboardStateNewWithConfig cfg = do
  iconWidget <- keyboardStateIconNewWithConfig cfg
  labelWidget <- keyboardStateLabelNewWithConfig cfg
  liftIO $ buildIconLabelBox iconWidget labelWidget

-- | Get the formatted keyboard state text.
getFormattedState :: KeyboardStateConfig -> IO T.Text
getFormattedState cfg =
  formatKeyboardState cfg <$> getKeyboardState
