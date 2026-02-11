{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.ASUS
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides a widget for displaying and controlling the current
-- ASUS platform profile along with CPU frequency and temperature.
--
-- Displays: @\<icon\> \<freq\> \<temp\>@, e.g. @nf-icon 3.2GHz 72°C@.
-- Left-click opens a profile selection menu; right-click cycles profiles.
-----------------------------------------------------------------------------
module System.Taffybar.Widget.ASUS
  ( ASUSWidgetConfig(..)
  , defaultASUSWidgetConfig
  , asusWidgetNew
  , asusWidgetNewWithConfig
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Default (Default(..))
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.Information.ASUS
import           System.Taffybar.Util (postGUIASync, logPrintF)
import           System.Taffybar.Widget.Generic.ChannelWidget
import           Text.Printf (printf)

-- | Configuration for the ASUS widget.
data ASUSWidgetConfig = ASUSWidgetConfig
  { asusQuietIcon       :: T.Text   -- ^ Nerd font icon for quiet mode
  , asusBalancedIcon    :: T.Text   -- ^ Nerd font icon for balanced mode
  , asusPerformanceIcon :: T.Text   -- ^ Nerd font icon for performance mode
  , asusShowFreq        :: Bool     -- ^ Show CPU frequency
  , asusShowTemp        :: Bool     -- ^ Show CPU temperature
  } deriving (Eq, Show)

-- | Default configuration.
-- Icons: nf-md-leaf (quiet), nf-md-scale_balance (balanced), nf-md-rocket_launch (performance)
defaultASUSWidgetConfig :: ASUSWidgetConfig
defaultASUSWidgetConfig = ASUSWidgetConfig
  { asusQuietIcon       = T.pack "\xF06C9"     -- nf-md-leaf
  , asusBalancedIcon    = T.pack "\xF0A7A"     -- nf-md-scale_balance
  , asusPerformanceIcon = T.pack "\xF0E4E"     -- nf-md-rocket_launch
  , asusShowFreq        = True
  , asusShowTemp        = True
  }

instance Default ASUSWidgetConfig where
  def = defaultASUSWidgetConfig

asusLogPath :: String
asusLogPath = "System.Taffybar.Widget.ASUS"

asusLogF :: (MonadIO m, Show t) => Priority -> String -> t -> m ()
asusLogF = logPrintF asusLogPath

-- | Create an ASUS widget with default configuration.
asusWidgetNew :: TaffyIO Gtk.Widget
asusWidgetNew = asusWidgetNewWithConfig defaultASUSWidgetConfig

-- | Create an ASUS widget with custom configuration.
asusWidgetNewWithConfig :: ASUSWidgetConfig -> TaffyIO Gtk.Widget
asusWidgetNewWithConfig config = do
  chan <- getASUSInfoChan
  ctx <- ask

  liftIO $ do
    label <- Gtk.labelNew Nothing
    ebox <- Gtk.eventBoxNew
    Gtk.containerAdd ebox label
    styleCtx <- Gtk.widgetGetStyleContext ebox
    Gtk.styleContextAddClass styleCtx "asus-profile"

    let updateWidget info = postGUIASync $ do
          let icon = getTextIcon config (asusProfile info)
              freqText = if asusShowFreq config
                         then T.pack $ printf " %.1fGHz" (asusCpuFreqGHz info)
                         else ""
              tempText = if asusShowTemp config
                         then T.pack $ printf " %.0f\x00B0C" (asusCpuTempC info)
                         else ""
              labelText = icon <> freqText <> tempText
          Gtk.labelSetText label labelText
          updateProfileClasses ebox info
          updateTooltip ebox info

    void $ Gtk.onWidgetRealize ebox $ do
      initialInfo <- runReaderT getASUSInfoState ctx
      updateWidget initialInfo

    setupClickHandler ctx ebox
    Gtk.widgetShowAll ebox
    Gtk.toWidget =<< channelWidgetNew ebox chan updateWidget

-- | Select the nerd font text icon for a given profile.
getTextIcon :: ASUSWidgetConfig -> ASUSPlatformProfile -> T.Text
getTextIcon config Quiet       = asusQuietIcon config
getTextIcon config Balanced    = asusBalancedIcon config
getTextIcon config Performance = asusPerformanceIcon config

-- | Update CSS classes based on the current profile.
updateProfileClasses :: Gtk.IsWidget w => w -> ASUSInfo -> IO ()
updateProfileClasses widget info = do
  let profile = asusProfile info
      allClasses = ["quiet", "balanced", "performance"] :: [T.Text]
      currentClass = case profile of
        Quiet       -> "quiet"
        Balanced    -> "balanced"
        Performance -> "performance"
  styleCtx <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextRemoveClass styleCtx) allClasses
  Gtk.styleContextAddClass styleCtx currentClass

-- | Update tooltip with current profile info.
updateTooltip :: Gtk.IsWidget w => w -> ASUSInfo -> IO ()
updateTooltip widget info = do
  let profile = asusProfileToString (asusProfile info)
      freqStr = T.pack $ printf "%.2f GHz" (asusCpuFreqGHz info)
      tempStr = T.pack $ printf "%.1f\x00B0C" (asusCpuTempC info)
      tooltipText = "Profile: " <> profile
                 <> "\nCPU Freq: " <> freqStr
                 <> "\nCPU Temp: " <> tempStr
  Gtk.widgetSetTooltipText widget (Just tooltipText)

-- | Set up click handler: left-click opens profile menu, right-click cycles.
setupClickHandler :: Context -> Gtk.EventBox -> IO ()
setupClickHandler ctx ebox = do
  void $ Gtk.onWidgetButtonPressEvent ebox $ \event -> do
    button <- Gdk.getEventButtonButton event
    eventType <- Gdk.getEventButtonType event
    if eventType /= Gdk.EventTypeButtonPress then return False
    else case button of
      1 -> do
        showProfileMenu ctx ebox
        return True
      3 -> do
        let client = systemDBusClient ctx
        result <- cycleASUSProfile client
        case result of
          Left err ->
            asusLogF WARNING "Failed to cycle ASUS profile: %s" (show err)
          Right () ->
            return ()
        return True
      _ -> return False

-- | Build and show a popup menu for selecting a profile.
showProfileMenu :: Context -> Gtk.EventBox -> IO ()
showProfileMenu ctx ebox = do
  currentEvent <- Gtk.getCurrentEvent
  currentInfo <- runReaderT getASUSInfoState ctx
  let currentProfile = asusProfile currentInfo

  menu <- Gtk.menuNew
  Gtk.menuAttachToWidget menu ebox Nothing

  let profiles = [ ("Quiet", Quiet)
                 , ("Balanced", Balanced)
                 , ("Performance", Performance)
                 ]

  forM_ profiles $ \(labelText, profile) -> do
    let prefix = if profile == currentProfile
                   then "\x2713 " :: T.Text
                   else "   "
    item <- Gtk.menuItemNewWithLabel (prefix <> labelText)
    void $ Gtk.onMenuItemActivate item $ do
      let client = systemDBusClient ctx
      result <- setASUSProfile client profile
      case result of
        Left err ->
          asusLogF WARNING "Failed to set ASUS profile: %s" (show err)
        Right () ->
          return ()
    Gtk.menuShellAppend menu item

  void $ Gtk.onWidgetHide menu $
    void $ GLib.idleAdd GLib.PRIORITY_LOW $ do
      Gtk.widgetDestroy menu
      return False

  Gtk.widgetShowAll menu
  Gtk.menuPopupAtPointer menu currentEvent
