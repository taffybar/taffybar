{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.BatteryTextIcon
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- A battery icon widget that uses text glyphs (e.g. Nerd Font icons) to
-- display battery state. The glyph changes based on charge level and whether
-- the battery is charging or discharging. CSS classes are applied for
-- additional styling.
module System.Taffybar.Widget.BatteryTextIcon
  ( batteryTextIconNew,
    batteryTextIconNewWith,
    BatteryTextIconConfig (..),
    defaultBatteryTextIconConfig,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Default (Default (..))
import qualified Data.Text as T
import GI.Gtk as Gtk
import System.Taffybar.Context
import System.Taffybar.Information.Battery
import System.Taffybar.Util
import System.Taffybar.Widget.Battery
import System.Taffybar.Widget.Generic.ChannelWidget
import System.Taffybar.Widget.Util hiding (themeLoadFlags)

-- | Configuration for the nerd font battery text icon widget.
data BatteryTextIconConfig = BatteryTextIconConfig
  { -- | Glyphs to use when discharging, ordered from empty (0%) to full (100%).
    -- Must have at least one element.
    batteryTextIconDischarging :: [T.Text],
    -- | Glyphs to use when charging, ordered from empty (0%) to full (100%).
    -- Must have at least one element.
    batteryTextIconCharging :: [T.Text],
    -- | Glyph to use when fully charged / on AC with no discharge.
    batteryTextIconFull :: T.Text
  }

-- | Default config using Nerd Font battery icons (Material Design Icons).
defaultBatteryTextIconConfig :: BatteryTextIconConfig
defaultBatteryTextIconConfig =
  BatteryTextIconConfig
    { batteryTextIconDischarging =
        [ "\xf008e", -- 󰂎 nf-md-battery_outline (0%)
          "\xf007a", -- 󰁺 nf-md-battery_10
          "\xf007b", -- 󰁻 nf-md-battery_20
          "\xf007c", -- 󰁼 nf-md-battery_30
          "\xf007d", -- 󰁽 nf-md-battery_40
          "\xf007e", -- 󰁾 nf-md-battery_50
          "\xf007f", -- 󰁿 nf-md-battery_60
          "\xf0080", -- 󰂀 nf-md-battery_70
          "\xf0081", -- 󰂁 nf-md-battery_80
          "\xf0082", -- 󰂂 nf-md-battery_90
          "\xf0079" -- 󰁹 nf-md-battery (100%)
        ],
      batteryTextIconCharging =
        [ "\xf089f", -- 󰢟 nf-md-battery_charging_outline (0%)
          "\xf089c", -- 󰢜 nf-md-battery_charging_10
          "\xf0086", -- 󰂆 nf-md-battery_charging_20
          "\xf0087", -- 󰂇 nf-md-battery_charging_30
          "\xf0088", -- 󰂈 nf-md-battery_charging_40
          "\xf089d", -- 󰢝 nf-md-battery_charging_50
          "\xf0089", -- 󰂉 nf-md-battery_charging_60
          "\xf089e", -- 󰢞 nf-md-battery_charging_70
          "\xf008a", -- 󰂊 nf-md-battery_charging_80
          "\xf008b", -- 󰂋 nf-md-battery_charging_90
          "\xf0085" -- 󰂅 nf-md-battery_charging_100
        ],
      batteryTextIconFull = "\xf0085" -- 󰂅 nf-md-battery_charging_100
    }

instance Default BatteryTextIconConfig where
  def = defaultBatteryTextIconConfig

-- | Select the appropriate glyph from a list based on battery percentage.
selectGlyph :: [T.Text] -> Double -> T.Text
selectGlyph [] _ = "?"
selectGlyph glyphs pct =
  let n = length glyphs
      idx = min (n - 1) $ floor (pct / 100.0 * fromIntegral n)
   in glyphs !! max 0 idx

-- | A battery icon widget that uses text glyphs (e.g. Nerd Font icons) to
-- display battery state. The glyph changes based on charge level and
-- charging\/discharging state. CSS classes @charging@, @discharging@, @high@,
-- @low@, and @critical@ are applied for styling.
--
-- Uses default Nerd Font Material Design battery icons. For custom glyphs, use
-- 'batteryTextIconNewWith'.
batteryTextIconNew :: TaffyIO Widget
batteryTextIconNew = batteryTextIconNewWith def

-- | Like 'batteryTextIconNew' but with a custom 'BatteryTextIconConfig'.
batteryTextIconNewWith :: BatteryTextIconConfig -> TaffyIO Widget
batteryTextIconNewWith config = do
  chan <- getDisplayBatteryChan
  ctx <- ask
  liftIO $ do
    label <- labelNew Nothing
    _ <- widgetSetClassGI label "battery-text-icon"
    let updateWidget info = postGUIASync $ do
          let pct = batteryPercentage info
              glyph = case batteryState info of
                BatteryStateCharging ->
                  selectGlyph (batteryTextIconCharging config) pct
                BatteryStateDischarging ->
                  selectGlyph (batteryTextIconDischarging config) pct
                _
                  | pct >= 99 -> batteryTextIconFull config
                  | otherwise ->
                      selectGlyph (batteryTextIconDischarging config) pct
          labelSetLabel label glyph
          setBatteryStateClasses def label info
    void $
      onWidgetRealize label $
        runReaderT getDisplayBatteryInfo ctx >>= updateWidget
    toWidget =<< channelWidgetNew label chan updateWidget
