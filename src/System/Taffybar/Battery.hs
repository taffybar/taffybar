{-# LANGUAGE OverloadedStrings #-}
-- | This module provides battery widgets using the UPower system
-- service.
--
-- Currently it reports only the first battery it finds.  If it does
-- not find a batterym it just returns an obnoxious widget with
-- warning text in it.  Battery hotplugging is not supported.  These
-- more advanced features could be supported if there is interest.
module System.Taffybar.Battery (
  batteryBarNew,
  textBatteryNew,
  defaultBatteryConfig
  ) where

import Data.Int
import Graphics.UI.Gtk
import Text.Printf
import Text.StringTemplate

import System.Information.Battery
import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingLabel

battInfo :: BatteryContext -> String -> IO String
battInfo ctxt fmt = do
  info <- getBatteryInfo ctxt
  let battPctNum :: Int
      battPctNum = floor (batteryPercentage info)
      formatTime :: Int64 -> String
      formatTime seconds =
        let minutes = seconds `div` 60
            hours = minutes `div` 60
            minutes' = minutes `mod` 60
        in printf "%02d:%02d" hours minutes'

      batteryOn :: Bool
      batteryOn = case (batteryState info) of
        BatteryStateCharging -> True
        BatteryStateFullyCharged -> True
        _ -> False


      battTime :: String
      battTime = case (batteryState info) of
        BatteryStateCharging -> (formatTime $ batteryTimeToFull info)
        BatteryStateDischarging -> (formatTime $ batteryTimeToEmpty info)
        _ -> "-"

      tpl = newSTMP fmt
      tpl' = setManyAttrib [ ("percentage", show battPctNum)
                           , ("time", battTime)
                           , ("percent_charge", if batteryOn then "On" else (show battPctNum) ++ "%")
                           ] tpl
  return $ render tpl'

-- | A simple textual battery widget that auto-updates once every
-- polling period (specified in seconds).  The displayed format is
-- specified format string where $percentage$ is replaced with the
-- percentage of battery remaining and $time$ is replaced with the
-- time until the battery is fully charged/discharged.
textBatteryNew :: String    -- ^ Display format
                  -> Double -- ^ Poll period in seconds
                  -> IO Widget
textBatteryNew fmt pollSeconds = do
  battCtxt <- batteryContextNew

  case battCtxt of
    Nothing -> labelNew (Just "No battery") >>= return . toWidget
    Just ctxt -> do
      l <- pollingLabelNew "" pollSeconds (battInfo ctxt fmt)
      widgetShowAll l
      return l

-- | Returns the current battery percent as a double in the range [0,
-- 1]
battPct :: BatteryContext -> IO Double
battPct ctxt = do
  info <- getBatteryInfo ctxt
  return (batteryPercentage info / 100)

-- | A default configuration for the graphical battery display.  The
-- bar will be red when power is critical (< 10%), green if it is full
-- (> 90%), and grey otherwise.
--
-- You can customize this with any of the options in 'BarConfig'
defaultBatteryConfig :: BarConfig
defaultBatteryConfig =
  defaultBarConfig colorFunc
  where
    colorFunc pct
      | pct < 0.1 = (1, 0, 0)
      | pct < 0.9 = (0.5, 0.5, 0.5)
      | otherwise = (0, 1, 0)

-- | A fancy graphical battery widget that represents the current
-- charge as a colored vertical bar.  There is also a textual
-- percentage readout next to the bar.
batteryBarNew :: BarConfig -- ^ Configuration options for the bar display
                 -> Double -- ^ Polling period in seconds
                 -> IO Widget
batteryBarNew battCfg pollSeconds = do
  battCtxt <- batteryContextNew
  case battCtxt of
    Nothing -> labelNew (Just "No battery") >>= return . toWidget
    Just ctxt -> do
      -- This is currently pretty inefficient - each poll period it
      -- queries the battery twice (once for the label and once for
      -- the bar).
      --
      -- Converting it to combine the two shouldn't be hard.
      b <- hBoxNew False 1
      txt <- textBatteryNew "$percent_charge$" pollSeconds
      bar <- pollingBarNew battCfg pollSeconds (battPct ctxt)
      boxPackStart b bar PackNatural 0
      boxPackStart b txt PackNatural 0
      widgetShowAll b
      return (toWidget b)
