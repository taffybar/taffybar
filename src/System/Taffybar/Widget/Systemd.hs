{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.Systemd
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides a widget that displays the number of failed systemd
-- units. It monitors both system and user units via DBus.
--
-- The widget shows a warning indicator with the total count of failed units.
-- When there are no failures, the widget can be configured to hide.
--
-- CSS classes applied:
--
-- * @systemd-widget@ - Always applied to the container
-- * @systemd-ok@ - Applied when there are no failed units
-- * @systemd-degraded@ - Applied when there are failed units
--
-- Example CSS:
--
-- > .systemd-widget { padding: 0 5px; }
-- > .systemd-ok { color: #98c379; }
-- > .systemd-degraded { color: #e06c75; }
-----------------------------------------------------------------------------
module System.Taffybar.Widget.Systemd
  ( SystemdConfig(..)
  , defaultSystemdConfig
  , systemdIconNew
  , systemdIconNewWithConfig
  , systemdLabelNew
  , systemdLabelNewWithConfig
  , systemdNew
  , systemdNewWithConfig
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Default (Default(..))
import qualified Data.Text as T
import           GI.Gtk as Gtk
import           System.Taffybar.Context
import           System.Taffybar.Information.Systemd
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.ChannelWidget
import           System.Taffybar.Widget.Util
import           Text.StringTemplate

-- | Configuration options for the systemd widget.
data SystemdConfig = SystemdConfig
  { -- | Format string when there are failed units.
    -- Available variables: $count$, $system$, $user$
    systemdFormat :: String
    -- | Format string when there are no failures (only used if hideOnOk is False).
  , systemdFormatOk :: String
    -- | Whether to hide the widget when there are no failed units.
  , systemdHideOnOk :: Bool
    -- | Whether to monitor system units.
  , systemdMonitorSystem :: Bool
    -- | Whether to monitor user units.
  , systemdMonitorUser :: Bool
    -- | Nerd font icon character (default U+F071, warning triangle).
  , systemdIcon :: T.Text
  } deriving (Eq, Show)

-- | Default configuration for the systemd widget.
defaultSystemdConfig :: SystemdConfig
defaultSystemdConfig = SystemdConfig
  { systemdFormat = "$count$"
  , systemdFormatOk = "\x2713"        -- Check mark
  , systemdHideOnOk = True
  , systemdMonitorSystem = True
  , systemdMonitorUser = True
  , systemdIcon = T.pack "\xF071"     -- Warning triangle
  }

instance Default SystemdConfig where
  def = defaultSystemdConfig

-- | Create a systemd label widget with default configuration.
systemdLabelNew :: TaffyIO Gtk.Widget
systemdLabelNew = systemdLabelNewWithConfig defaultSystemdConfig

-- | Create a systemd label widget with custom configuration.
systemdLabelNewWithConfig :: SystemdConfig -> TaffyIO Gtk.Widget
systemdLabelNewWithConfig config = do
  chan <- getSystemdInfoChan
  ctx <- ask
  liftIO $ do
    label <- labelNew Nothing
    _ <- widgetSetClassGI label "systemd-widget"

    let updateWidget info = postGUIASync $ do
          let effectiveCount = computeEffectiveCount config info
              isOk = effectiveCount == 0
          updateSystemdLabel config label info
          updateSystemdVisibility config label isOk
          updateSystemdClasses label isOk

    -- Set initial state on realize
    void $ onWidgetRealize label $ do
      info <- runReaderT getSystemdInfoState ctx
      let effectiveCount = computeEffectiveCount config info
          isOk = effectiveCount == 0
      updateSystemdLabel config label info
      updateSystemdVisibility config label isOk
      updateSystemdClasses label isOk

    toWidget =<< channelWidgetNew label chan updateWidget

-- | Create a systemd icon widget with default configuration.
systemdIconNew :: TaffyIO Gtk.Widget
systemdIconNew = systemdIconNewWithConfig defaultSystemdConfig

-- | Create a systemd icon widget with the provided configuration.
systemdIconNewWithConfig :: SystemdConfig -> TaffyIO Gtk.Widget
systemdIconNewWithConfig config = liftIO $ do
  label <- Gtk.labelNew (Just (systemdIcon config))
  Gtk.widgetShowAll label
  Gtk.toWidget label

-- | Create a combined icon+label systemd widget with default configuration.
systemdNew :: TaffyIO Gtk.Widget
systemdNew = systemdNewWithConfig defaultSystemdConfig

-- | Create a combined icon+label systemd widget with custom configuration.
systemdNewWithConfig :: SystemdConfig -> TaffyIO Gtk.Widget
systemdNewWithConfig config = do
  iconWidget <- systemdIconNewWithConfig config
  labelWidget <- systemdLabelNewWithConfig config
  liftIO $ buildIconLabelBox iconWidget labelWidget

-- | Compute the effective count based on configuration.
computeEffectiveCount :: SystemdConfig -> SystemdInfo -> Int
computeEffectiveCount config info =
  let sysCount = if systemdMonitorSystem config then systemFailedCount info else 0
      usrCount = if systemdMonitorUser config then userFailedCount info else 0
  in sysCount + usrCount

-- | Update the label text based on the current state.
updateSystemdLabel :: SystemdConfig -> Gtk.Label -> SystemdInfo -> IO ()
updateSystemdLabel config label info = do
  let effectiveCount = computeEffectiveCount config info
      isOk = effectiveCount == 0
      formatStr = if isOk then systemdFormatOk config else systemdFormat config
      tpl = newSTMP formatStr
      tpl' = setManyAttrib
        [ ("count", show effectiveCount)
        , ("system", show (systemFailedCount info))
        , ("user", show (userFailedCount info))
        ] tpl
  labelSetMarkup label (render tpl')

-- | Update widget visibility based on configuration.
updateSystemdVisibility :: SystemdConfig -> Gtk.Label -> Bool -> IO ()
updateSystemdVisibility config label isOk =
  if isOk && systemdHideOnOk config
    then widgetHide label
    else widgetShow label

-- | Update CSS classes based on state.
updateSystemdClasses :: Gtk.Label -> Bool -> IO ()
updateSystemdClasses label isOk = do
  if isOk
    then do
      addClassIfMissing "systemd-ok" label
      removeClassIfPresent "systemd-degraded" label
    else do
      addClassIfMissing "systemd-degraded" label
      removeClassIfPresent "systemd-ok" label
