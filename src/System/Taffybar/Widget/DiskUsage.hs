{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.DiskUsage
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- A label widget that displays disk usage information, backed by a shared
-- polling thread from "System.Taffybar.Information.DiskUsage".
--
-- Template variables: @$total$@, @$used$@, @$free$@, @$available$@,
-- @$usedPercent$@, @$freePercent$@, @$path$@.
--
-- Size values are auto-formatted with appropriate units (GiB, TiB, etc.).
module System.Taffybar.Widget.DiskUsage
  ( DiskUsageWidgetConfig (..),
    defaultDiskUsageWidgetConfig,
    diskUsageIconNew,
    diskUsageIconNewWith,
    diskUsageLabelNew,
    diskUsageLabelNewWith,
    diskUsageNew,
    diskUsageNewWith,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Default (Default (..))
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.DiskUsage
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget
import System.Taffybar.Widget.Util (buildIconLabelBox)
import Text.Printf (printf)
import Text.StringTemplate

-- | Configuration for disk usage widgets.
data DiskUsageWidgetConfig = DiskUsageWidgetConfig
  { -- | Filesystem path to monitor (default @\/@).
    diskUsagePath :: FilePath,
    -- | Polling interval in seconds (default 60).
    diskUsagePollInterval :: Double,
    -- | Label format string (default @\"$free$\"@).
    diskUsageFormat :: String,
    -- | Optional tooltip format string.
    diskUsageTooltipFormat :: Maybe String,
    -- | Nerd font icon character (default U+F0A0, ).
    diskUsageIcon :: T.Text
  }

-- | Default disk usage widget configuration.
defaultDiskUsageWidgetConfig :: DiskUsageWidgetConfig
defaultDiskUsageWidgetConfig =
  DiskUsageWidgetConfig
    { diskUsagePath = "/",
      diskUsagePollInterval = 60,
      diskUsageFormat = "$free$",
      diskUsageTooltipFormat =
        Just "$path$: $used$ / $total$ ($usedPercent$% used)",
      diskUsageIcon = T.pack "\xF0A0" --
    }

instance Default DiskUsageWidgetConfig where
  def = defaultDiskUsageWidgetConfig

-- | Create a disk usage label with default settings (monitors @/@, 60s poll).
diskUsageLabelNew :: TaffyIO Gtk.Widget
diskUsageLabelNew = diskUsageLabelNewWith defaultDiskUsageWidgetConfig

-- | Create a disk usage label with the given configuration.
diskUsageLabelNewWith :: DiskUsageWidgetConfig -> TaffyIO Gtk.Widget
diskUsageLabelNewWith config = do
  let path = diskUsagePath config
      interval = diskUsagePollInterval config
  chan <- getDiskUsageInfoChan interval path
  initialInfo <- getDiskUsageInfoState interval path

  liftIO $ do
    label <- Gtk.labelNew Nothing

    let updateLabel info = postGUIASync $ do
          let (labelText, tooltipText) = formatDiskUsage config info
          Gtk.labelSetText label labelText
          Gtk.widgetSetTooltipText label tooltipText

    void $ Gtk.onWidgetRealize label $ updateLabel initialInfo
    Gtk.widgetShowAll label
    Gtk.toWidget =<< channelWidgetNew label chan updateLabel

-- | Create a disk usage icon widget with default configuration.
diskUsageIconNew :: TaffyIO Gtk.Widget
diskUsageIconNew = diskUsageIconNewWith defaultDiskUsageWidgetConfig

-- | Create a disk usage icon widget with the provided configuration.
diskUsageIconNewWith :: DiskUsageWidgetConfig -> TaffyIO Gtk.Widget
diskUsageIconNewWith config = liftIO $ do
  label <- Gtk.labelNew (Just (diskUsageIcon config))
  Gtk.widgetShowAll label
  Gtk.toWidget label

-- | Create a combined icon+label disk usage widget with default configuration.
diskUsageNew :: TaffyIO Gtk.Widget
diskUsageNew = diskUsageNewWith defaultDiskUsageWidgetConfig

-- | Create a combined icon+label disk usage widget.
diskUsageNewWith :: DiskUsageWidgetConfig -> TaffyIO Gtk.Widget
diskUsageNewWith config = do
  iconWidget <- diskUsageIconNewWith config
  labelWidget <- diskUsageLabelNewWith config
  liftIO $ buildIconLabelBox iconWidget labelWidget

-- --------------------------------------------------------------------------
-- Formatting

formatDiskUsage :: DiskUsageWidgetConfig -> DiskUsageInfo -> (T.Text, Maybe T.Text)
formatDiskUsage config info =
  let attrs = diskUsageAttrs (diskUsagePath config) info
      labelText = renderTpl (diskUsageFormat config) attrs
      tooltipText = renderTpl <$> diskUsageTooltipFormat config <*> pure attrs
   in (labelText, tooltipText)

renderTpl :: String -> [(String, String)] -> T.Text
renderTpl template attrs =
  T.pack $ render $ setManyAttrib attrs (newSTMP template)

diskUsageAttrs :: FilePath -> DiskUsageInfo -> [(String, String)]
diskUsageAttrs path info =
  [ ("total", formatBytes (diskInfoTotal info)),
    ("used", formatBytes (diskInfoUsed info)),
    ("free", formatBytes (diskInfoFree info)),
    ("available", formatBytes (diskInfoAvailable info)),
    ("usedPercent", printf "%.0f" (diskInfoUsedPercent info)),
    ("freePercent", printf "%.0f" (diskInfoFreePercent info)),
    ("path", path)
  ]

-- | Format a byte count with auto-scaled units.
formatBytes :: Integer -> String
formatBytes bytes
  | gb >= 1024 = printf "%.1f TiB" (gb / 1024 :: Double)
  | gb >= 1 = printf "%.1f GiB" gb
  | mb >= 1 = printf "%.0f MiB" mb
  | otherwise = printf "%.0f KiB" kb
  where
    kb = fromIntegral bytes / 1024 :: Double
    mb = kb / 1024
    gb = mb / 1024
