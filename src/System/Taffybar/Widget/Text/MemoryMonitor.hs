module System.Taffybar.Widget.Text.MemoryMonitor (textMemoryMonitorNew, showMemoryInfo) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import qualified GI.Gtk
import System.Taffybar.Information.Memory
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNew)
import Text.Printf (printf)
import qualified Text.StringTemplate as ST

-- | Creates a simple textual memory monitor. It updates once every polling
-- period (in seconds).
textMemoryMonitorNew ::
  (MonadIO m) =>
  -- | Format. You can use variables: "used", "total", "free", "buffer",
  -- "cache", "rest", "available", "swapUsed", "swapTotal", "swapFree",
  -- "usedRatio", "swapUsedRatio", "usedPercent", "swapUsedPercent".
  String ->
  -- | Polling period in seconds.
  Double ->
  m GI.Gtk.Widget
textMemoryMonitorNew fmt period = do
  label <- pollingLabelNew period (showMemoryInfo fmt 3 <$> parseMeminfo)
  GI.Gtk.toWidget label

showMemoryInfo :: String -> Int -> MemoryInfo -> T.Text
showMemoryInfo fmt prec info =
  let template = ST.newSTMP fmt
      sizeLabels =
        [ "used",
          "total",
          "free",
          "buffer",
          "cache",
          "rest",
          "available",
          "swapUsed",
          "swapTotal",
          "swapFree"
        ]
      sizeActions =
        [ memoryUsed,
          memoryTotal,
          memoryFree,
          memoryBuffer,
          memoryCache,
          memoryRest,
          memoryAvailable,
          memorySwapUsed,
          memorySwapTotal,
          memorySwapFree
        ]
      sizeStats =
        [ (label, toAuto prec (action info))
        | (label, action) <- zip sizeLabels sizeActions
        ]
      ratioStats =
        [ ("usedRatio", toRatio prec (memoryUsedRatio info)),
          ("swapUsedRatio", toRatio prec (memorySwapUsedRatio info))
        ]
      percentStats =
        [ ("usedPercent", toPercent 1 (memoryUsedRatio info)),
          ("swapUsedPercent", toPercent 1 (memorySwapUsedRatio info))
        ]
      template' = ST.setManyAttrib (sizeStats ++ ratioStats ++ percentStats) template
   in ST.render template'

toAuto :: Int -> Double -> String
toAuto prec value = printf "%.*f%s" p v unit
  where
    value' = max 0 value
    mag :: Int
    mag = if value' == 0 then 0 else max 0 $ min 2 $ floor $ logBase 1024 value'
    v = value' / 1024 ** fromIntegral mag
    unit = case mag of
      0 -> "MiB"
      1 -> "GiB"
      2 -> "TiB"
      _ -> "??B" -- unreachable
    p :: Int
    p = max 0 $ floor $ fromIntegral prec - logBase 10 v

toRatio :: Int -> Double -> String
toRatio prec value = printf "%.*f" p value'
  where
    p = max 0 prec
    value' = max 0 value

toPercent :: Int -> Double -> String
toPercent prec value = printf "%.*f%%" p (value' * 100)
  where
    p = max 0 prec
    value' = max 0 value
