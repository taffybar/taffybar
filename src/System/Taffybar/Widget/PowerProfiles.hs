{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.PowerProfiles
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides widgets for displaying and controlling the current
-- power profile using the power-profiles-daemon DBus service.
--
-- Three widget variants are provided:
--
--   * 'powerProfilesIconNew' -- a dynamic nerd-font text icon
--   * 'powerProfilesLabelNew' -- a text label showing the profile name
--   * 'powerProfilesNew' -- combined icon + label
--
-- All variants cycle through profiles on left-click.
module System.Taffybar.Widget.PowerProfiles
  ( PowerProfilesConfig (..),
    defaultPowerProfilesConfig,
    powerProfilesIconNew,
    powerProfilesIconNewWithConfig,
    powerProfilesLabelNew,
    powerProfilesLabelNewWithConfig,
    powerProfilesNew,
    powerProfilesNewWithConfig,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Default (Default (..))
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import System.Log.Logger
import System.Taffybar.Context
import System.Taffybar.Information.PowerProfiles
import System.Taffybar.Util (logPrintF, postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget
import System.Taffybar.Widget.Util (buildIconLabelBox)

-- | Configuration for the power profiles widget.
data PowerProfilesConfig = PowerProfilesConfig
  { -- | Format string for the label. Use @$profile$@ for the profile name.
    powerProfilesFormat :: String,
    -- | Nerd font text icon for power-saver mode (default U+F06C9, nf-md-leaf).
    powerProfilesPowerSaverTextIcon :: T.Text,
    -- | Nerd font text icon for balanced mode (default U+F0A7A, nf-md-scale_balance).
    powerProfilesBalancedTextIcon :: T.Text,
    -- | Nerd font text icon for performance mode (default U+F0E4E, nf-md-rocket_launch).
    powerProfilesPerformanceTextIcon :: T.Text
  }
  deriving (Eq, Show)

-- | Default configuration for the power profiles widget.
defaultPowerProfilesConfig :: PowerProfilesConfig
defaultPowerProfilesConfig =
  PowerProfilesConfig
    { powerProfilesFormat = "$profile$",
      powerProfilesPowerSaverTextIcon = T.pack "\xF06C9",
      powerProfilesBalancedTextIcon = T.pack "\xF0A7A",
      powerProfilesPerformanceTextIcon = T.pack "\xF0E4E"
    }

instance Default PowerProfilesConfig where
  def = defaultPowerProfilesConfig

powerProfilesLogPath :: String
powerProfilesLogPath = "System.Taffybar.Widget.PowerProfiles"

powerProfilesLogF :: (MonadIO m, Show t) => Priority -> String -> t -> m ()
powerProfilesLogF = logPrintF powerProfilesLogPath

-- ---------------------------------------------------------------------------
-- Icon-only widget (dynamic nerd font text icon)

-- | Create a power profiles icon widget with default configuration.
powerProfilesIconNew :: TaffyIO Gtk.Widget
powerProfilesIconNew = powerProfilesIconNewWithConfig defaultPowerProfilesConfig

-- | Create a power profiles icon widget with custom configuration.
--
-- The icon is a nerd-font text label that updates dynamically when the
-- active profile changes. CSS classes @power-saver@, @balanced@, and
-- @performance@ are toggled on the label.
powerProfilesIconNewWithConfig :: PowerProfilesConfig -> TaffyIO Gtk.Widget
powerProfilesIconNewWithConfig config = do
  chan <- getPowerProfileInfoChan
  ctx <- ask
  liftIO $ do
    label <- Gtk.labelNew Nothing
    let updateIcon info = postGUIASync $ do
          let iconText = getTextIcon config (currentProfile info)
          Gtk.labelSetText label iconText
          updateProfileClasses label info
    void $ Gtk.onWidgetRealize label $ do
      initialInfo <- runReaderT getPowerProfileInfoState ctx
      updateIcon initialInfo
    Gtk.widgetShowAll label
    Gtk.toWidget =<< channelWidgetNew label chan updateIcon

-- ---------------------------------------------------------------------------
-- Label-only widget (text label with format string)

-- | Create a power profiles label widget with default configuration.
powerProfilesLabelNew :: TaffyIO Gtk.Widget
powerProfilesLabelNew = powerProfilesLabelNewWithConfig defaultPowerProfilesConfig

-- | Create a power profiles label widget with custom configuration.
--
-- The label displays the profile name formatted via 'powerProfilesFormat'.
-- CSS classes @power-saver@, @balanced@, and @performance@ are toggled on
-- the label.
powerProfilesLabelNewWithConfig :: PowerProfilesConfig -> TaffyIO Gtk.Widget
powerProfilesLabelNewWithConfig config = do
  chan <- getPowerProfileInfoChan
  ctx <- ask
  liftIO $ do
    label <- Gtk.labelNew Nothing
    let updateLabel info = postGUIASync $ do
          let profileName = T.unpack $ powerProfileToString (currentProfile info)
              labelText = formatLabel (powerProfilesFormat config) profileName
          Gtk.labelSetMarkup label (T.pack labelText)
          updateProfileClasses label info
    void $ Gtk.onWidgetRealize label $ do
      initialInfo <- runReaderT getPowerProfileInfoState ctx
      updateLabel initialInfo
    Gtk.widgetShowAll label
    Gtk.toWidget =<< channelWidgetNew label chan updateLabel

-- ---------------------------------------------------------------------------
-- Combined icon + label widget

-- | Create a combined icon + label power profiles widget with default
-- configuration.
powerProfilesNew :: TaffyIO Gtk.Widget
powerProfilesNew = powerProfilesNewWithConfig defaultPowerProfilesConfig

-- | Create a combined icon + label power profiles widget.
--
-- Wraps the icon and label in a horizontal box (via 'buildIconLabelBox')
-- inside an event box with click-to-cycle and tooltip. CSS classes
-- @power-profiles@, @power-saver@, @balanced@, and @performance@ are
-- applied to the event box.
powerProfilesNewWithConfig :: PowerProfilesConfig -> TaffyIO Gtk.Widget
powerProfilesNewWithConfig config = do
  chan <- getPowerProfileInfoChan
  ctx <- ask

  iconWidget <- powerProfilesIconNewWithConfig config
  labelWidget <- powerProfilesLabelNewWithConfig config

  liftIO $ do
    box <- buildIconLabelBox iconWidget labelWidget
    ebox <- Gtk.eventBoxNew
    Gtk.containerAdd ebox box
    styleCtx <- Gtk.widgetGetStyleContext ebox
    Gtk.styleContextAddClass styleCtx "power-profiles"

    let updateWidget info = postGUIASync $ do
          updateProfileClasses ebox info
          updateTooltip ebox info

    void $ Gtk.onWidgetRealize ebox $ do
      initialInfo <- runReaderT getPowerProfileInfoState ctx
      updateWidget initialInfo

    setupClickHandler ctx ebox
    Gtk.widgetShowAll ebox
    Gtk.toWidget =<< channelWidgetNew ebox chan updateWidget

-- ---------------------------------------------------------------------------
-- Helpers

-- | Select the nerd font text icon for a given profile.
getTextIcon :: PowerProfilesConfig -> PowerProfile -> T.Text
getTextIcon config PowerSaver = powerProfilesPowerSaverTextIcon config
getTextIcon config Balanced = powerProfilesBalancedTextIcon config
getTextIcon config Performance = powerProfilesPerformanceTextIcon config

-- | Format the label string, replacing @$profile$@ with the profile name.
formatLabel :: String -> String -> String
formatLabel fmt profile = replace "$profile$" profile fmt
  where
    replace :: String -> String -> String -> String
    replace old new = T.unpack . T.replace (T.pack old) (T.pack new) . T.pack

-- | Update CSS classes based on the current profile. Works with any widget.
updateProfileClasses :: (Gtk.IsWidget w) => w -> PowerProfileInfo -> IO ()
updateProfileClasses widget info = do
  let profile = currentProfile info
      allClasses = ["power-saver", "balanced", "performance"]
      currentClass = case profile of
        PowerSaver -> "power-saver"
        Balanced -> "balanced"
        Performance -> "performance"
  styleCtx <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextRemoveClass styleCtx) allClasses
  Gtk.styleContextAddClass styleCtx currentClass

-- | Update tooltip with current profile info.
updateTooltip :: (Gtk.IsWidget w) => w -> PowerProfileInfo -> IO ()
updateTooltip widget info = do
  let profile = currentProfile info
      profileName = powerProfileToString profile
      degradedText = case performanceDegraded info of
        Nothing -> ""
        Just reason -> "\nPerformance degraded: " <> reason
      tooltipText = "Power Profile: " <> profileName <> degradedText
  Gtk.widgetSetTooltipText widget (Just tooltipText)

-- | Set up click handler to cycle profiles on left-click.
setupClickHandler :: Context -> Gtk.EventBox -> IO ()
setupClickHandler ctx ebox = do
  void $ Gtk.onWidgetButtonPressEvent ebox $ \event -> do
    button <- Gdk.getEventButtonButton event
    eventType <- Gdk.getEventButtonType event
    if eventType == Gdk.EventTypeButtonPress && button == 1
      then do
        info <- runReaderT getPowerProfileInfoState ctx
        client <- readSystemDBusClient ctx
        result <- cycleProfile client info
        case result of
          Left err ->
            powerProfilesLogF WARNING "Failed to cycle power profile: %s" (show err)
          Right () ->
            return ()
        return True
      else return False
