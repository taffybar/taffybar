{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Battery
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides battery widgets that are queried using the UPower dbus
-- service. To avoid duplicating all information requests for each battery
-- widget displayed (if using a multi-head configuration or multiple battery
-- widgets), these widgets use the broadcast "TChan" based system for receiving
-- updates defined in "System.Taffybar.Information.Battery".
module System.Taffybar.Widget.Battery
  ( batteryIconNew,
    BatteryClassesConfig (..),
    defaultBatteryClassesConfig,
    setBatteryStateClasses,
    textBatteryNew,
    textBatteryNewWithLabelAction,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Default (Default (..))
import Data.GI.Base.Overloading (IsDescendantOf)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.Text as T
import GI.Gtk as Gtk
import System.Taffybar.Context
import System.Taffybar.Information.Battery
import System.Taffybar.Util
import System.Taffybar.Widget.Generic.ChannelWidget
import System.Taffybar.Widget.Generic.ScalingImage (scalingImage)
import System.Taffybar.Widget.Util hiding (themeLoadFlags)
import Text.Printf
import Text.StringTemplate

-- | Just the battery info that will be used for display (this makes combining
-- several easier).
data BatteryWidgetInfo = BWI
  { seconds :: Maybe Int64,
    percent :: Int,
    status :: String
  }
  deriving (Eq, Show)

-- | Format a duration expressed as seconds to hours and minutes
formatDuration :: Maybe Int64 -> String
formatDuration Nothing = ""
formatDuration (Just secs) =
  let minutes = secs `div` 60
      hours = minutes `div` 60
      minutes' = minutes `mod` 60
   in printf "%02d:%02d" hours minutes'

getBatteryWidgetInfo :: BatteryInfo -> BatteryWidgetInfo
getBatteryWidgetInfo info =
  let battPctNum :: Int
      battPctNum = floor (batteryPercentage info)
      battTime :: Maybe Int64
      battTime =
        case batteryState info of
          BatteryStateCharging -> Just $ batteryTimeToFull info
          BatteryStateDischarging -> Just $ batteryTimeToEmpty info
          _ -> Nothing
      battStatus :: String
      battStatus =
        case batteryState info of
          BatteryStateCharging -> "Charging"
          BatteryStateDischarging -> "Discharging"
          _ -> "✔"
   in BWI {seconds = battTime, percent = battPctNum, status = battStatus}

-- | Given (maybe summarized) battery info and format: provides the string to display
formatBattInfo :: BatteryWidgetInfo -> String -> T.Text
formatBattInfo info fmt =
  let tpl = newSTMP fmt
      tpl' =
        setManyAttrib
          [ ("percentage", (show . percent) info),
            ("time", formatDuration (seconds info)),
            ("status", status info)
          ]
          tpl
   in render tpl'

-- | A simple textual battery widget. The displayed format is specified format
-- string where $percentage$ is replaced with the percentage of battery
-- remaining and $time$ is replaced with the time until the battery is fully
-- charged/discharged.
textBatteryNew :: String -> TaffyIO Widget
textBatteryNew format = textBatteryNewWithLabelAction labelSetter
  where
    labelSetter label info = do
      setBatteryStateClasses def label info
      labelSetMarkup label $
        formatBattInfo (getBatteryWidgetInfo info) format

-- | CSS-threshold configuration for battery level classes.
data BatteryClassesConfig = BatteryClassesConfig
  { batteryHighThreshold :: Double,
    batteryLowThreshold :: Double,
    batteryCriticalThreshold :: Double
  }

-- | Default thresholds for 'BatteryClassesConfig'.
defaultBatteryClassesConfig :: BatteryClassesConfig
defaultBatteryClassesConfig =
  BatteryClassesConfig
    { batteryHighThreshold = 80,
      batteryLowThreshold = 20,
      batteryCriticalThreshold = 5
    }

instance Default BatteryClassesConfig where
  def = defaultBatteryClassesConfig

-- | Add/remove CSS classes on a widget to reflect battery charging state and
-- level thresholds.
setBatteryStateClasses ::
  (IsDescendantOf Widget a, GObject a, MonadIO m) =>
  BatteryClassesConfig -> a -> BatteryInfo -> m ()
setBatteryStateClasses config widget info = do
  case batteryState info of
    BatteryStateCharging ->
      addClassIfMissing "charging" widget
        >> removeClassIfPresent "discharging" widget
    BatteryStateDischarging ->
      addClassIfMissing "discharging" widget
        >> removeClassIfPresent "charging" widget
    _ ->
      removeClassIfPresent "charging" widget
        >> removeClassIfPresent "discharging" widget

  classIf "high" $ percentage >= batteryHighThreshold config
  classIf "low" $ percentage <= batteryLowThreshold config
  classIf "critical" $ percentage <= batteryCriticalThreshold config
  where
    percentage = batteryPercentage info
    classIf klass condition =
      if condition
        then addClassIfMissing klass widget
        else removeClassIfPresent klass widget

-- | Like `textBatteryNew` but provides a more general way to update the label
-- widget. The argument provided is an action that is used to update the text
-- label given a 'BatteryInfo' object describing the state of the battery.
textBatteryNewWithLabelAction ::
  (Gtk.Label -> BatteryInfo -> TaffyIO ()) -> TaffyIO Widget
textBatteryNewWithLabelAction labelSetter = do
  chan <- getDisplayBatteryChan
  ctx <- ask
  liftIO $ do
    label <- labelNew Nothing
    let updateWidget =
          postGUIASync . flip runReaderT ctx . labelSetter label
    void $
      onWidgetRealize label $
        runReaderT getDisplayBatteryInfo ctx >>= updateWidget
    toWidget =<< channelWidgetNew label chan updateWidget

themeLoadFlags :: [IconLookupFlags]
themeLoadFlags = [IconLookupFlagsGenericFallback, IconLookupFlagsUseBuiltin]

-- | Create an icon-only battery widget using the battery icon name from UPower.
batteryIconNew :: TaffyIO Widget
batteryIconNew = do
  chan <- getDisplayBatteryChan
  ctx <- ask
  defaultTheme <- liftIO iconThemeGetDefault
  imageWidgetRef <- liftIO $ newIORef (error "imageWidget not initialised")
  let setIconForSize size = do
        iw <- readIORef imageWidgetRef
        styleCtx <- widgetGetStyleContext iw
        name <- T.pack . batteryIconName <$> runReaderT getDisplayBatteryInfo ctx
        iconThemeLookupIcon defaultTheme name size themeLoadFlags
          >>= traverse (\info -> fst <$> iconInfoLoadSymbolicForContext info styleCtx)
  (imageWidget, updateImage) <- scalingImage setIconForSize OrientationHorizontal
  liftIO $ do
    writeIORef imageWidgetRef imageWidget
    toWidget =<< channelWidgetNew imageWidget chan (const $ postGUIASync updateImage)
