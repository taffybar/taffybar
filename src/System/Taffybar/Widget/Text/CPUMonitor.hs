{-# LANGUAGE NamedFieldPuns #-}

-- | Textual CPU monitor widgets.
module System.Taffybar.Widget.Text.CPUMonitor (textCpuMonitorNew) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified GI.Gtk
import System.Taffybar.Information.CPU2 (CPULoad (..), getCPULoadChan)
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)
import Text.Printf (printf)
import qualified Text.StringTemplate as ST

-- | Creates a simple textual CPU monitor. It updates once every polling
-- period (in seconds).
textCpuMonitorNew ::
  (MonadIO m) =>
  -- | Format. You can use variables: $total$, $user$, $system$
  String ->
  -- | Polling period (in seconds)
  Double ->
  m GI.Gtk.Widget
textCpuMonitorNew fmt period = liftIO $ do
  chan <- getCPULoadChan "cpu" period
  label <- GI.Gtk.labelNew Nothing
  void $
    channelWidgetNew label chan $ \sample ->
      postGUIASync $
        GI.Gtk.labelSetMarkup label $
          renderCpuInfo fmt sample
  GI.Gtk.toWidget label

renderCpuInfo :: String -> CPULoad -> T.Text
renderCpuInfo fmt CPULoad {cpuUserLoad, cpuSystemLoad, cpuTotalLoad} =
  let pct = formatPercent . (* 100)
      template = ST.newSTMP fmt
      template' =
        ST.setManyAttrib
          [ ("user", pct cpuUserLoad),
            ("system", pct cpuSystemLoad),
            ("total", pct cpuTotalLoad)
          ]
          template
   in T.pack $ ST.render template'

formatPercent :: Double -> String
formatPercent = printf "%.2f"
