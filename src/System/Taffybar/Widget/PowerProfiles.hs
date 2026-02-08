{-# LANGUAGE OverloadedStrings #-}
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
-- This module provides a widget for displaying and controlling the current
-- power profile using the power-profiles-daemon DBus service.
--
-- The widget shows an icon representing the current profile and allows
-- cycling through profiles by clicking on it.
-----------------------------------------------------------------------------
module System.Taffybar.Widget.PowerProfiles
  ( powerProfilesNew
  , powerProfilesNewWithConfig
  , PowerProfilesConfig(..)
  , defaultPowerProfilesConfig
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Default (Default(..))
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.Information.PowerProfiles
import           System.Taffybar.Util (postGUIASync, logPrintF)
import           System.Taffybar.Widget.Generic.ChannelWidget
import           System.Taffybar.Widget.Util

-- | Configuration for the power profiles widget.
data PowerProfilesConfig = PowerProfilesConfig
  { -- | Format string for the label. Use $profile$ for the profile name.
    powerProfilesFormat :: String
  , -- | Icon name for power-saver mode.
    powerProfilesPowerSaverIcon :: T.Text
  , -- | Icon name for balanced mode.
    powerProfilesBalancedIcon :: T.Text
  , -- | Icon name for performance mode.
    powerProfilesPerformanceIcon :: T.Text
  , -- | Whether to show an icon (True) or text label (False).
    powerProfilesShowIcon :: Bool
  } deriving (Eq, Show)

-- | Default configuration for the power profiles widget.
defaultPowerProfilesConfig :: PowerProfilesConfig
defaultPowerProfilesConfig = PowerProfilesConfig
  { powerProfilesFormat = "$profile$"
  , powerProfilesPowerSaverIcon = "power-profile-power-saver-symbolic"
  , powerProfilesBalancedIcon = "power-profile-balanced-symbolic"
  , powerProfilesPerformanceIcon = "power-profile-performance-symbolic"
  , powerProfilesShowIcon = True
  }

instance Default PowerProfilesConfig where
  def = defaultPowerProfilesConfig

powerProfilesLogPath :: String
powerProfilesLogPath = "System.Taffybar.Widget.PowerProfiles"

powerProfilesLogF :: (MonadIO m, Show t) => Priority -> String -> t -> m ()
powerProfilesLogF = logPrintF powerProfilesLogPath

-- | Create a power profiles widget with default configuration.
powerProfilesNew :: TaffyIO Gtk.Widget
powerProfilesNew = powerProfilesNewWithConfig defaultPowerProfilesConfig

-- | Create a power profiles widget with custom configuration.
powerProfilesNewWithConfig :: PowerProfilesConfig -> TaffyIO Gtk.Widget
powerProfilesNewWithConfig config = do
  chan <- getPowerProfileInfoChan
  ctx <- ask

  liftIO $ do
    -- Create the widget container
    ebox <- Gtk.eventBoxNew
    _ <- widgetSetClassGI ebox "power-profiles"

    -- Create either an image or label based on config
    if powerProfilesShowIcon config
      then do
        image <- Gtk.imageNew
        Gtk.containerAdd ebox image

        -- Update function for icon mode
        let updateWidget info = postGUIASync $ do
              let iconName = getIconName config (currentProfile info)
              Gtk.imageSetFromIconName image (Just iconName) $
                fromIntegral (fromEnum Gtk.IconSizeMenu)
              updateClasses ebox info
              updateTooltip ebox info

        -- Initialize on realize
        void $ Gtk.onWidgetRealize ebox $ do
          initialInfo <- runReaderT getPowerProfileInfoState ctx
          updateWidget initialInfo

        -- Set up click handler
        setupClickHandler ctx ebox

        -- Connect to channel for updates
        Gtk.toWidget =<< channelWidgetNew ebox chan updateWidget

      else do
        label <- Gtk.labelNew Nothing
        Gtk.containerAdd ebox label

        -- Update function for label mode
        let updateWidget info = postGUIASync $ do
              let profileName = T.unpack $ powerProfileToString (currentProfile info)
                  labelText = formatLabel (powerProfilesFormat config) profileName
              Gtk.labelSetMarkup label (T.pack labelText)
              updateClasses ebox info
              updateTooltip ebox info

        -- Initialize on realize
        void $ Gtk.onWidgetRealize ebox $ do
          initialInfo <- runReaderT getPowerProfileInfoState ctx
          updateWidget initialInfo

        -- Set up click handler
        setupClickHandler ctx ebox

        -- Connect to channel for updates
        Gtk.toWidget =<< channelWidgetNew ebox chan updateWidget

-- | Format the label string with profile name.
formatLabel :: String -> String -> String
formatLabel fmt profile = replace "$profile$" profile fmt
  where
    replace :: String -> String -> String -> String
    replace old new = T.unpack . T.replace (T.pack old) (T.pack new) . T.pack

-- | Get the icon name for a profile.
getIconName :: PowerProfilesConfig -> PowerProfile -> T.Text
getIconName config PowerSaver = powerProfilesPowerSaverIcon config
getIconName config Balanced = powerProfilesBalancedIcon config
getIconName config Performance = powerProfilesPerformanceIcon config

-- | Update CSS classes based on current profile.
updateClasses :: Gtk.EventBox -> PowerProfileInfo -> IO ()
updateClasses widget info = do
  let profile = currentProfile info
      allClasses = ["power-saver", "balanced", "performance"]
      currentClass = case profile of
        PowerSaver -> "power-saver"
        Balanced -> "balanced"
        Performance -> "performance"
  styleCtx <- Gtk.widgetGetStyleContext widget
  -- Remove all profile classes first
  mapM_ (Gtk.styleContextRemoveClass styleCtx) allClasses
  -- Add current profile class
  Gtk.styleContextAddClass styleCtx currentClass

-- | Update tooltip with current profile info.
updateTooltip :: Gtk.EventBox -> PowerProfileInfo -> IO ()
updateTooltip widget info = do
  let profile = currentProfile info
      profileName = powerProfileToString profile
      degradedText = case performanceDegraded info of
        Nothing -> ""
        Just reason -> "\nPerformance degraded: " <> reason
      tooltipText = "Power Profile: " <> profileName <> degradedText
  Gtk.widgetSetTooltipText widget (Just tooltipText)

-- | Set up click handler to cycle profiles.
setupClickHandler :: Context -> Gtk.EventBox -> IO ()
setupClickHandler ctx ebox = do
  void $ Gtk.onWidgetButtonPressEvent ebox $ \event -> do
    button <- Gdk.getEventButtonButton event
    eventType <- Gdk.getEventButtonType event
    if eventType == Gdk.EventTypeButtonPress && button == 1
      then do
        -- Left click - cycle to next profile
        info <- runReaderT getPowerProfileInfoState ctx
        let client = systemDBusClient ctx
        result <- cycleProfile client info
        case result of
          Left err ->
            powerProfilesLogF WARNING "Failed to cycle power profile: %s" (show err)
          Right () ->
            return ()
        return True
      else return False
