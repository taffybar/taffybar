{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.BatteryDonut
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- A battery widget that draws a donut\/arc using Cairo to visualize battery
-- charge level. The arc color changes based on configurable charge thresholds.
module System.Taffybar.Widget.BatteryDonut
  ( batteryDonutNew,
    batteryDonutNewWith,
    batteryDonutLabelNew,
    batteryDonutLabelNewWith,
    BatteryDonutConfig (..),
    defaultBatteryDonutConfig,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Default (Default (..))
import Data.IORef
import Data.Int (Int32)
import qualified GI.Cairo.Render as C
import GI.Cairo.Render.Connector (renderWithContext)
import GI.Gtk as Gtk
import System.Taffybar.Context
import System.Taffybar.Information.Battery
import System.Taffybar.Util
import System.Taffybar.Widget.Battery (textBatteryNew)
import System.Taffybar.Widget.Generic.ChannelWidget
import System.Taffybar.Widget.Util
  ( addClassIfMissing,
    buildIconLabelBox,
    removeClassIfPresent,
    widgetSetClassGI,
  )
import qualified System.Taffybar.Widget.Util as WU

-- | Configuration for the donut/arc battery widget.
data BatteryDonutConfig = BatteryDonutConfig
  { -- | Color when charge is high (default: green)
    donutHighColor :: (Double, Double, Double),
    -- | Color when charge is medium (default: yellow)
    donutMediumColor :: (Double, Double, Double),
    -- | Color when charge is low (default: orange)
    donutLowColor :: (Double, Double, Double),
    -- | Color when charge is critical (default: red)
    donutCriticalColor :: (Double, Double, Double),
    -- | Background ring color (default: dim gray)
    donutBackgroundColor :: (Double, Double, Double),
    -- | Percentage above which color is "high" (default: 60)
    donutHighThreshold :: Double,
    -- | Percentage above which color is "medium" (default: 30)
    donutMediumThreshold :: Double,
    -- | Percentage below which color is "critical" (default: 10)
    donutCriticalThreshold :: Double,
    -- | Requested widget size in pixels (default: 24)
    donutSize :: Int32,
    -- | Optional override color when charging (default: Nothing)
    donutChargingColor :: Maybe (Double, Double, Double),
    -- | Format string for the text label in the combined donut+label variant.
    -- Uses the same template syntax as 'textBatteryNew': @$percentage$@,
    -- @$time$@, and @$status$@ are replaced with the corresponding values.
    -- (default: @"$percentage$%"@)
    donutLabelFormat :: String
  }

-- | Default donut battery configuration.
defaultBatteryDonutConfig :: BatteryDonutConfig
defaultBatteryDonutConfig =
  BatteryDonutConfig
    { donutHighColor = (0.3, 0.85, 0.3),
      donutMediumColor = (0.95, 0.85, 0.2),
      donutLowColor = (0.95, 0.5, 0.1),
      donutCriticalColor = (0.9, 0.2, 0.2),
      donutBackgroundColor = (0.3, 0.3, 0.3),
      donutHighThreshold = 60,
      donutMediumThreshold = 30,
      donutCriticalThreshold = 10,
      donutSize = 24,
      donutChargingColor = Nothing,
      donutLabelFormat = "$percentage$%"
    }

instance Default BatteryDonutConfig where
  def = defaultBatteryDonutConfig

-- | Select the arc color based on battery percentage and charging state.
donutColorForLevel :: BatteryDonutConfig -> Double -> BatteryState -> (Double, Double, Double)
donutColorForLevel BatteryDonutConfig {..} pct state =
  case (state, donutChargingColor) of
    (BatteryStateCharging, Just color) -> color
    _
      | pct >= donutHighThreshold -> donutHighColor
      | pct >= donutMediumThreshold -> donutMediumColor
      | pct >= donutCriticalThreshold -> donutLowColor
      | otherwise -> donutCriticalColor

-- | Render the donut arc for the battery.
renderDonut :: BatteryDonutConfig -> Double -> BatteryState -> Int -> Int -> C.Render ()
renderDonut cfg pct state w h = do
  let size = fromIntegral (min w h) :: Double
      cx = fromIntegral w / 2
      cy = fromIntegral h / 2
      lineW = size * 0.22
      radius = (size - lineW) / 2
      startAngle = -(pi / 2)
      fullAngle = 2 * pi
      fraction = pct / 100
      endAngle = startAngle + fullAngle * fraction
      (bgR, bgG, bgB) = donutBackgroundColor cfg
      (fgR, fgG, fgB) = donutColorForLevel cfg pct state

  -- Draw background ring
  C.setLineCap C.LineCapRound
  C.setLineWidth lineW
  C.setSourceRGB bgR bgG bgB
  C.arc cx cy radius 0 fullAngle
  C.stroke

  -- Draw foreground arc (only if there is something to draw)
  when (fraction > 0.001) $ do
    C.setSourceRGB fgR fgG fgB
    C.arc cx cy radius startAngle endAngle
    C.stroke

-- | Set CSS classes on the donut widget based on battery state.
setDonutBatteryClasses ::
  (Gtk.IsWidget w, MonadIO m) => BatteryDonutConfig -> w -> BatteryInfo -> m ()
setDonutBatteryClasses cfg widget info = do
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

  classIf "high" $ percentage >= donutHighThreshold cfg
  classIf "medium" $
    percentage >= donutMediumThreshold cfg
      && percentage < donutHighThreshold cfg
  classIf "low" $
    percentage < donutMediumThreshold cfg
      && percentage >= donutCriticalThreshold cfg
  classIf "critical" $ percentage < donutCriticalThreshold cfg
  where
    percentage = batteryPercentage info
    classIf klass condition =
      if condition
        then addClassIfMissing klass widget
        else removeClassIfPresent klass widget

-- | Create a donut/arc battery widget with the default configuration.
batteryDonutNew :: TaffyIO Widget
batteryDonutNew = batteryDonutNewWith def

-- | Create a donut/arc battery widget with a custom configuration.
batteryDonutNewWith :: BatteryDonutConfig -> TaffyIO Widget
batteryDonutNewWith cfg = do
  chan <- getDisplayBatteryChan
  ctx <- ask
  liftIO $ do
    drawArea <- drawingAreaNew
    widgetSetSizeRequest drawArea (donutSize cfg) (donutSize cfg)

    pctRef <- newIORef 0.0
    stateRef <- newIORef BatteryStateUnknown

    _ <- widgetSetClassGI drawArea "battery-donut"

    _ <- onWidgetDraw drawArea $ \cairoCtx -> do
      (w, h) <- WU.widgetGetAllocatedSize drawArea
      pct <- readIORef pctRef
      bState <- readIORef stateRef
      renderWithContext (renderDonut cfg pct bState w h) cairoCtx
      return True

    let updateWidget info = postGUIASync $ do
          writeIORef pctRef (batteryPercentage info)
          writeIORef stateRef (batteryState info)
          setDonutBatteryClasses cfg drawArea info
          widgetQueueDraw drawArea

    void $
      onWidgetRealize drawArea $
        runReaderT getDisplayBatteryInfo ctx >>= updateWidget

    toWidget =<< channelWidgetNew drawArea chan updateWidget

-- | Create a combined donut icon + text label battery widget with the default
-- configuration.
batteryDonutLabelNew :: TaffyIO Widget
batteryDonutLabelNew = batteryDonutLabelNewWith def

-- | Create a combined donut icon + text label battery widget with a custom
-- configuration.
batteryDonutLabelNewWith :: BatteryDonutConfig -> TaffyIO Widget
batteryDonutLabelNewWith config = do
  iconWidget <- batteryDonutNewWith config
  labelWidget <- textBatteryNew (donutLabelFormat config)
  liftIO $ buildIconLabelBox iconWidget labelWidget
