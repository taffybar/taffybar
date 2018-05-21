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
-- This module provides battery widgets using the UPower system
-- service.
--
-- Currently it reports only the first battery it finds. If it does not find a
-- battery, it just returns an obnoxious widget with warning text in it. Battery
-- hotplugging is not supported. These more advanced features could be supported
-- if there is interest.
-----------------------------------------------------------------------------
module System.Taffybar.Widget.Battery ( textBatteryNew, batteryIconNew ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.GI.Gtk.Threading
import           Data.Int (Int64)
import qualified Data.Text as T
import           GI.Gtk
import qualified Graphics.UI.Gtk as Gtk2hs
import           Prelude
import           StatusNotifier.Tray (scalePixbufToSize)
import           System.Taffybar.Compat.GtkLibs
import           System.Taffybar.Context
import           System.Taffybar.Information.Battery
import           System.Taffybar.Widget.Generic.AutoSizeImage
import           System.Taffybar.Widget.Generic.ChannelWidget
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
formatBattInfo :: BatteryWidgetInfo -> String -> String
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
textBatteryNew
  :: String -- ^ Display format
  -> TaffyIO Gtk2hs.Widget
textBatteryNew format = fromGIWidget =<< do
  chan <- getDisplayBatteryChan
  ctx <- ask
  let getLabelText info =
        T.pack $ formatBattInfo (getBatteryWidgetInfo info) format
      getBatteryInfoIO = runReaderT getDisplayBatteryInfo ctx
  liftIO $ do
    label <- getLabelText <$> getBatteryInfoIO >>= labelNew . Just
    let setMarkup text = postGUIASync $ labelSetMarkup label text
        updateWidget = setMarkup . getLabelText
    void $ onWidgetRealize label $ getLabelText <$> getBatteryInfoIO >>= setMarkup
    toWidget =<< channelWidgetNew label chan updateWidget

themeLoadFlags :: [IconLookupFlags]
themeLoadFlags = [IconLookupFlagsGenericFallback, IconLookupFlagsUseBuiltin]

batteryIconNew :: TaffyIO Gtk2hs.Widget
batteryIconNew = fromGIWidget =<< do
  chan <- getDisplayBatteryChan
  ctx <- ask
  liftIO $ do
    image <- imageNew
    defaultTheme <- iconThemeGetDefault
    let getCurrentBatteryIconNameString =
          T.pack . batteryIconName <$> runReaderT getDisplayBatteryInfo ctx
        setIconForSize size = do
          name <- getCurrentBatteryIconNameString
          iconThemeLoadIcon defaultTheme name size themeLoadFlags >>=
                            traverse (scalePixbufToSize size OrientationHorizontal)
    updateImage <- autoSizeImage image setIconForSize OrientationHorizontal
    toWidget =<< channelWidgetNew image chan (const $ postGUIASync updateImage)
