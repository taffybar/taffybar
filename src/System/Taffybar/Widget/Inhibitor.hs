{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Inhibitor
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides a widget for controlling idle/sleep inhibitors.
-- The widget displays the current inhibitor state and allows toggling
-- the inhibitor on/off with a click.
--
-- Example usage:
--
-- > import System.Taffybar.Widget.Inhibitor
-- >
-- > main = do
-- >   -- Simple inhibitor widget that inhibits idle
-- >   let inhibitor = inhibitorNew
-- >
-- >   -- Or with custom configuration
-- >   let customInhibitor = inhibitorNewWithConfig defaultInhibitorConfig
-- >         { inhibitWhat = [InhibitIdle, InhibitSleep]
-- >         , inhibitorActiveText = "AWAKE"
-- >         , inhibitorInactiveText = "zzz"
-- >         }
module System.Taffybar.Widget.Inhibitor
  ( -- * Widget constructors
    inhibitorNew,
    inhibitorNewWithConfig,
    inhibitorIconNew,
    inhibitorIconNewWithConfig,
    inhibitorLabelNew,
    inhibitorLabelNewWithConfig,

    -- * Configuration
    InhibitorConfig (..),
    defaultInhibitorConfig,

    -- * Re-exports
    InhibitType (..),
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import GI.Gtk as Gtk
import System.Taffybar.Context
import System.Taffybar.Information.Inhibitor
import System.Taffybar.Util
import System.Taffybar.Widget.Generic.ChannelWidget
import System.Taffybar.Widget.Util

-- | Configuration for the inhibitor widget
data InhibitorConfig = InhibitorConfig
  { -- | What types of inhibitors to manage (default: [InhibitIdle])
    inhibitWhat :: [InhibitType],
    -- | Text to display when inhibitor is active
    inhibitorActiveText :: T.Text,
    -- | Text to display when inhibitor is inactive
    inhibitorInactiveText :: T.Text,
    -- | Icon to display when inhibitor is active (default: U+F0F4, nf-fa-coffee)
    inhibitorActiveIcon :: T.Text,
    -- | Icon to display when inhibitor is inactive (default: U+F236, nf-fa-bed)
    inhibitorInactiveIcon :: T.Text,
    -- | CSS class prefix (results in "prefix-active" and "prefix-inactive")
    inhibitorCssPrefix :: T.Text
  }
  deriving (Eq, Show)

-- | Default configuration: inhibits idle, shows simple icon-style text
defaultInhibitorConfig :: InhibitorConfig
defaultInhibitorConfig =
  InhibitorConfig
    { inhibitWhat = [InhibitIdle],
      inhibitorActiveText = "INHIBIT",
      inhibitorInactiveText = "inhibit",
      inhibitorActiveIcon = T.pack "\xF0F4",
      inhibitorInactiveIcon = T.pack "\xF236",
      inhibitorCssPrefix = "inhibitor"
    }

-- | Create a combined icon+label inhibitor widget with default configuration
inhibitorNew :: TaffyIO Widget
inhibitorNew = inhibitorNewWithConfig defaultInhibitorConfig

-- | Create a combined icon+label inhibitor widget with custom configuration.
-- The icon changes dynamically based on inhibitor state.
-- Click to toggle the inhibitor on/off.
inhibitorNewWithConfig :: InhibitorConfig -> TaffyIO Widget
inhibitorNewWithConfig config = do
  let types = inhibitWhat config
  iconWidget <- inhibitorIconNewWithConfig config
  labelWidget <- inhibitorLabelNewWithConfig config
  ctx <- ask
  liftIO $ do
    box <- buildIconLabelBox iconWidget labelWidget
    ebox <- eventBoxNew
    containerAdd ebox box

    _ <- widgetSetClassGI ebox "inhibitor"

    _ <- onWidgetButtonPressEvent ebox $ \event -> do
      button <- Gdk.getEventButtonButton event
      when (button == 1) $
        runReaderT (toggleInhibitor types) ctx
      return True

    widgetShowAll ebox
    toWidget ebox

-- | Create an icon-only inhibitor widget with default configuration
inhibitorIconNew :: TaffyIO Widget
inhibitorIconNew = inhibitorIconNewWithConfig defaultInhibitorConfig

-- | Create an icon-only inhibitor widget with custom configuration.
-- The icon changes dynamically based on inhibitor state.
inhibitorIconNewWithConfig :: InhibitorConfig -> TaffyIO Widget
inhibitorIconNewWithConfig config = do
  let types = inhibitWhat config
  chan <- getInhibitorChan types
  ctx <- ask
  liftIO $ do
    label <- labelNew Nothing
    let updateIcon state =
          postGUIASync $
            labelSetText label $
              if inhibitorActive state
                then inhibitorActiveIcon config
                else inhibitorInactiveIcon config
    void $ onWidgetRealize label $ do
      initialState <- runReaderT (getInhibitorState types) ctx
      updateIcon initialState
    widgetShowAll label
    toWidget =<< channelWidgetNew label chan updateIcon

-- | Create a label-only inhibitor widget with default configuration.
-- Click to toggle the inhibitor on/off.
inhibitorLabelNew :: TaffyIO Widget
inhibitorLabelNew = inhibitorLabelNewWithConfig defaultInhibitorConfig

-- | Create a label-only inhibitor widget with custom configuration.
-- Displays text and CSS classes based on inhibitor state.
-- Click to toggle the inhibitor on/off.
inhibitorLabelNewWithConfig :: InhibitorConfig -> TaffyIO Widget
inhibitorLabelNewWithConfig config = do
  let types = inhibitWhat config
  chan <- getInhibitorChan types
  ctx <- ask
  liftIO $ do
    label <- labelNew Nothing
    ebox <- eventBoxNew

    -- Set initial CSS class
    _ <- widgetSetClassGI ebox "inhibitor"

    containerAdd ebox label

    -- Set up click handler
    _ <- onWidgetButtonPressEvent ebox $ \event -> do
      button <- Gdk.getEventButtonButton event
      when (button == 1) $
        runReaderT (toggleInhibitor types) ctx
      return True

    -- Update function for the widget
    let updateWidget state = postGUIASync $ do
          let (text, activeClass, inactiveClass) =
                if inhibitorActive state
                  then
                    ( inhibitorActiveText config,
                      inhibitorCssPrefix config <> "-active",
                      inhibitorCssPrefix config <> "-inactive"
                    )
                  else
                    ( inhibitorInactiveText config,
                      inhibitorCssPrefix config <> "-inactive",
                      inhibitorCssPrefix config <> "-active"
                    )
          labelSetText label text
          addClassIfMissing activeClass ebox
          removeClassIfPresent inactiveClass ebox

    -- Set initial state
    void $ onWidgetRealize ebox $ do
      initialState <- runReaderT (getInhibitorState types) ctx
      updateWidget initialState

    -- Connect to state changes
    toWidget =<< channelWidgetNew ebox chan updateWidget
