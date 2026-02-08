{-# LANGUAGE OverloadedStrings #-}
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
-----------------------------------------------------------------------------

module System.Taffybar.Widget.DiskUsage
  ( DiskUsageWidgetConfig(..)
  , defaultDiskUsageWidgetConfig
  , diskUsageLabelNew
  , diskUsageLabelNewWith
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Default (Default(..))
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.DiskUsage
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget
import Text.Printf (printf)
import Text.StringTemplate

data DiskUsageWidgetConfig = DiskUsageWidgetConfig
  { diskUsagePath         :: FilePath
  -- ^ Filesystem path to monitor (default @\/@).
  , diskUsagePollInterval :: Double
  -- ^ Polling interval in seconds (default 60).
  , diskUsageFormat       :: String
  -- ^ Label format string (default @\"$free$\"@).
  , diskUsageTooltipFormat :: Maybe String
  -- ^ Optional tooltip format string.
  }

defaultDiskUsageWidgetConfig :: DiskUsageWidgetConfig
defaultDiskUsageWidgetConfig = DiskUsageWidgetConfig
  { diskUsagePath         = "/"
  , diskUsagePollInterval = 60
  , diskUsageFormat       = "$free$"
  , diskUsageTooltipFormat =
      Just "$path$: $used$ / $total$ ($usedPercent$% used)"
  }

instance Default DiskUsageWidgetConfig where
  def = defaultDiskUsageWidgetConfig

-- | Create a disk usage label with default settings (monitors @/@, 60s poll).
diskUsageLabelNew :: TaffyIO Gtk.Widget
diskUsageLabelNew = diskUsageLabelNewWith defaultDiskUsageWidgetConfig

-- | Create a disk usage label with the given configuration.
diskUsageLabelNewWith :: DiskUsageWidgetConfig -> TaffyIO Gtk.Widget
diskUsageLabelNewWith config = do
  let path     = diskUsagePath config
      interval = diskUsagePollInterval config
  chan        <- getDiskUsageInfoChan interval path
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

-- --------------------------------------------------------------------------
-- Formatting

formatDiskUsage :: DiskUsageWidgetConfig -> DiskUsageInfo -> (T.Text, Maybe T.Text)
formatDiskUsage config info =
  let attrs = diskUsageAttrs (diskUsagePath config) info
      labelText   = renderTpl (diskUsageFormat config) attrs
      tooltipText = renderTpl <$> diskUsageTooltipFormat config <*> pure attrs
  in (labelText, tooltipText)

renderTpl :: String -> [(String, String)] -> T.Text
renderTpl template attrs =
  T.pack $ render $ setManyAttrib attrs (newSTMP template)

diskUsageAttrs :: FilePath -> DiskUsageInfo -> [(String, String)]
diskUsageAttrs path info =
  [ ("total",       formatBytes (diskInfoTotal info))
  , ("used",        formatBytes (diskInfoUsed info))
  , ("free",        formatBytes (diskInfoFree info))
  , ("available",   formatBytes (diskInfoAvailable info))
  , ("usedPercent", printf "%.0f" (diskInfoUsedPercent info))
  , ("freePercent", printf "%.0f" (diskInfoFreePercent info))
  , ("path",        path)
  ]

-- | Format a byte count with auto-scaled units.
formatBytes :: Integer -> String
formatBytes bytes
  | gb >= 1024 = printf "%.1f TiB" (gb / 1024 :: Double)
  | gb >= 1    = printf "%.1f GiB" gb
  | mb >= 1    = printf "%.0f MiB" mb
  | otherwise  = printf "%.0f KiB" kb
  where
    kb = fromIntegral bytes / 1024        :: Double
    mb = kb / 1024
    gb = mb / 1024
