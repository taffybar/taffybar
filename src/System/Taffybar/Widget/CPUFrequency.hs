{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : System.Taffybar.Widget.CPUFrequency
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- A generic Linux CPU clock-speed widget backed by shared information state.
module System.Taffybar.Widget.CPUFrequency
  ( CPUFrequencyWidgetConfig (..),
    defaultCPUFrequencyWidgetConfig,
    cpuFrequencyNew,
    cpuFrequencyNewWithConfig,
  )
where

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Default (Default (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.CPUFrequency
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)
import System.Taffybar.Widget.Util (buildIconLabelBox, widgetSetClassGI)
import Text.Printf (printf)

data CPUFrequencyWidgetConfig = CPUFrequencyWidgetConfig
  { cpuFrequencyPollInterval :: Double,
    cpuFrequencyIcon :: T.Text
  }
  deriving (Eq, Show)

defaultCPUFrequencyWidgetConfig :: CPUFrequencyWidgetConfig
defaultCPUFrequencyWidgetConfig =
  CPUFrequencyWidgetConfig
    { cpuFrequencyPollInterval = 10,
      cpuFrequencyIcon = "\xF2DB" -- Font Awesome: microchip
    }

instance Default CPUFrequencyWidgetConfig where
  def = defaultCPUFrequencyWidgetConfig

cpuFrequencyNew :: TaffyIO Gtk.Widget
cpuFrequencyNew = cpuFrequencyNewWithConfig defaultCPUFrequencyWidgetConfig

cpuFrequencyNewWithConfig :: CPUFrequencyWidgetConfig -> TaffyIO Gtk.Widget
cpuFrequencyNewWithConfig config = do
  let interval = cpuFrequencyPollInterval config
  chan <- getCPUFrequencyInfoChan interval
  initialInfo <- getCPUFrequencyInfoState interval
  liftIO $ do
    icon <- Gtk.toWidget =<< Gtk.labelNew (Just $ cpuFrequencyIcon config)
    valueLabel <- Gtk.labelNew Nothing
    value <- Gtk.toWidget valueLabel
    row <- buildIconLabelBox icon value
    _ <- widgetSetClassGI row "cpu-frequency"
    renderedRef <- newIORef Nothing

    let updateWidget info = do
          let rendered = renderCPUFrequency info
          previous <- readIORef renderedRef
          when (previous /= Just rendered) $ do
            writeIORef renderedRef $ Just rendered
            postGUIASync $ do
              Gtk.labelSetText valueLabel $ fst rendered
              Gtk.widgetSetTooltipText row $ Just $ snd rendered

    void $ Gtk.onWidgetRealize row $ updateWidget initialInfo
    Gtk.widgetShowAll row
    Gtk.toWidget =<< channelWidgetNew row chan updateWidget

renderCPUFrequency :: CPUFrequencyInfo -> (T.Text, T.Text)
renderCPUFrequency info =
  case cpuFrequencyAverageGHz info of
    Nothing -> ("n/a", "CPU clock speed unavailable")
    Just average ->
      ( T.pack $ printf "%.1fGHz" average,
        T.pack $
          printf
            "CPU clock average: %.2f GHz\nRange: %.2f-%.2f GHz\n%d frequency policies sampled"
            average
            (fromMaybe average $ cpuFrequencyMinimumGHz info)
            (fromMaybe average $ cpuFrequencyMaximumGHz info)
            (cpuFrequencySampleCount info)
      )
