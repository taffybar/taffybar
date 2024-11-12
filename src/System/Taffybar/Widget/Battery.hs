{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- widgets), these widgets use the "BroadcastChan" based system for receiving
-- updates defined in "System.Taffybar.Information.Battery".
-----------------------------------------------------------------------------
module System.Taffybar.Widget.Battery
  ( batteryIconNew
  , textBatteryNew
  , textBatteryNewWithLabelAction
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Default (Default(..))
import           Data.Int (Int64)
import qualified Data.Text as T
import           GI.Gtk as Gtk
import           StatusNotifier.Tray (scalePixbufToSize)
import           System.Taffybar.Context
import           System.Taffybar.Information.Battery
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.AutoSizeImage
import           System.Taffybar.Widget.Generic.ChannelWidget
import           System.Taffybar.Widget.Util hiding (themeLoadFlags)
import           Text.Printf
import           Text.StringTemplate

-- | Just the battery info that will be used for display (this makes combining
-- several easier).
data BatteryWidgetInfo = BWI
  { seconds :: Maybe Int64
  , percent :: Int
  , status :: String
  } deriving (Eq, Show)

-- | Format a duration expressed as seconds to hours and minutes
formatDuration :: Maybe Int64 -> String
formatDuration Nothing = ""
formatDuration (Just secs) = let minutes = secs `div` 60
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
      tpl' = setManyAttrib [ ("percentage", (show . percent) info)
                           , ("time", formatDuration (seconds info))
                           , ("status", status info)
                           ] tpl
  in render tpl'

-- | A simple textual battery widget. The displayed format is specified format
-- string where $percentage$ is replaced with the percentage of battery
-- remaining and $time$ is replaced with the time until the battery is fully
-- charged/discharged.
textBatteryNew :: String -> TaffyIO Widget
textBatteryNew format = textBatteryNewWithLabelAction labelSetter
  where labelSetter label info = do
          setBatteryStateClasses def label info
          labelSetMarkup label $
                         formatBattInfo (getBatteryWidgetInfo info) format

data BatteryClassesConfig = BatteryClassesConfig
  { batteryHighThreshold :: Double
  , batteryLowThreshold :: Double
  , batteryCriticalThreshold :: Double
  }

defaultBatteryClassesConfig :: BatteryClassesConfig
defaultBatteryClassesConfig =
  BatteryClassesConfig
  { batteryHighThreshold = 80
  , batteryLowThreshold = 20
  , batteryCriticalThreshold = 5
  }

instance Default BatteryClassesConfig where
  def = defaultBatteryClassesConfig

setBatteryStateClasses ::
  MonadIO m => BatteryClassesConfig -> Gtk.Label -> BatteryInfo -> m ()
setBatteryStateClasses config label info = do
  case batteryState info of
    BatteryStateCharging -> addClassIfMissing "charging" label >>
                            removeClassIfPresent "discharging" label
    BatteryStateDischarging -> addClassIfMissing "discharging" label >>
                               removeClassIfPresent "charging" label
    _ -> removeClassIfPresent "charging" label >>
         removeClassIfPresent "discharging" label

  classIf "high" $ percentage >= batteryHighThreshold config
  classIf "low" $ percentage <= batteryLowThreshold config
  classIf "critical" $ percentage <= batteryCriticalThreshold config
  where percentage = batteryPercentage info
        classIf klass condition =
          if condition
          then addClassIfMissing klass label
          else removeClassIfPresent klass label

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
          postGUIASync . runTaffy ctx . labelSetter label
    void $ onWidgetRealize label $
         runTaffy ctx getDisplayBatteryInfo >>= updateWidget
    toWidget =<< channelWidgetNew label chan updateWidget

themeLoadFlags :: [IconLookupFlags]
themeLoadFlags = [IconLookupFlagsGenericFallback, IconLookupFlagsUseBuiltin]

batteryIconNew :: TaffyIO Widget
batteryIconNew = do
  chan <- getDisplayBatteryChan
  ctx <- ask
  liftIO $ do
    image <- imageNew
    styleCtx <- widgetGetStyleContext =<< toWidget image
    defaultTheme <- iconThemeGetDefault
    let getCurrentBatteryIconNameString =
          T.pack . batteryIconName <$> runTaffy ctx getDisplayBatteryInfo
        extractPixbuf info =
          fst <$> iconInfoLoadSymbolicForContext info styleCtx
        setIconForSize size = do
          name <- getCurrentBatteryIconNameString
          iconThemeLookupIcon defaultTheme name size themeLoadFlags >>=
            traverse extractPixbuf >>=
              traverse (scalePixbufToSize size OrientationHorizontal)
    updateImage <- autoSizeImage image setIconForSize OrientationHorizontal
    toWidget =<< channelWidgetNew image chan (const $ postGUIASync updateImage)
