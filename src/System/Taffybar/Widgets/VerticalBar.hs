-- | A vertical bar that can plot data in the range [0, 1].  The
-- colors are configurable.
module System.Taffybar.Widgets.VerticalBar (
  -- * Types
  VerticalBarHandle,
  BarConfig(..),
  BarDirection(..),
  -- * Accessors/Constructors
  verticalBarNew,
  verticalBarSetPercent,
  defaultBarConfig,
  defaultBarConfigIO
  ) where

import Control.Concurrent
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk

newtype VerticalBarHandle = VBH (MVar VerticalBarState)
data VerticalBarState =
  VerticalBarState { barIsBootstrapped :: Bool
                   , barPercent :: Double
                   , barCanvas :: DrawingArea
                   , barConfig :: BarConfig
                   }

data BarDirection = HORIZONTAL | VERTICAL

data BarConfig =
  BarConfig { barBorderColor :: (Double, Double, Double) -- ^ Color of the border drawn around the widget
            , barBackgroundColor :: Double -> (Double, Double, Double) -- ^ The background color of the widget
            , barColor :: Double -> (Double, Double, Double) -- ^ A function to determine the color of the widget for the current data point
            , barPadding :: Int -- ^ Number of pixels of padding around the widget
            , barWidth :: Int
            , barDirection :: BarDirection
            }
  | BarConfigIO { barBorderColorIO :: IO (Double, Double, Double)
                , barBackgroundColorIO :: Double -> IO (Double, Double, Double)
                , barColorIO :: Double -> IO (Double, Double, Double)
                , barPadding :: Int
                , barWidth :: Int
                , barDirection :: BarDirection
                }

-- | A default bar configuration.  The color of the active portion of
-- the bar must be specified.
defaultBarConfig :: (Double -> (Double, Double, Double)) -> BarConfig
defaultBarConfig c = BarConfig { barBorderColor = (0.5, 0.5, 0.5)
                               , barBackgroundColor = const (0, 0, 0)
                               , barColor = c
                               , barPadding = 2
                               , barWidth = 15
                               , barDirection = VERTICAL
                               }

defaultBarConfigIO :: (Double -> IO (Double, Double, Double)) -> BarConfig
defaultBarConfigIO c = BarConfigIO { barBorderColorIO = return (0.5, 0.5, 0.5)
                                   , barBackgroundColorIO = \_ -> return (0, 0, 0)
                                   , barColorIO = c
                                   , barPadding = 2
                                   , barWidth = 15
                                   , barDirection = VERTICAL
                                 }

verticalBarSetPercent :: VerticalBarHandle -> Double -> IO ()
verticalBarSetPercent (VBH mv) pct = do
  s <- readMVar mv
  let drawArea = barCanvas s
  case barIsBootstrapped s of
    False -> return ()
    True -> do
      modifyMVar_ mv (\s' -> return s' { barPercent = clamp 0 1 pct })
      postGUIAsync $ widgetQueueDraw drawArea

clamp :: Double -> Double -> Double -> Double
clamp lo hi d = max lo $ min hi d

liftedBackgroundColor :: BarConfig -> Double -> IO (Double, Double, Double)
liftedBackgroundColor bc pct =
  case bc of
    BarConfig { barBackgroundColor = bcolor } -> return (bcolor pct)
    BarConfigIO { barBackgroundColorIO = bcolor } -> bcolor pct

liftedBorderColor :: BarConfig -> IO (Double, Double, Double)
liftedBorderColor bc =
  case bc of
    BarConfig { barBorderColor = border } -> return border
    BarConfigIO { barBorderColorIO = border } -> border

liftedBarColor :: BarConfig -> Double -> IO (Double, Double, Double)
liftedBarColor bc pct =
  case bc of
    BarConfig { barColor = c } -> return (c pct)
    BarConfigIO { barColorIO = c } -> c pct

renderFrame :: Double -> BarConfig -> Int -> Int -> C.Render ()
renderFrame pct cfg width height = do
  let fwidth = fromIntegral width
      fheight = fromIntegral height

  -- Now draw the user's requested background, respecting padding
  (bgR, bgG, bgB) <- C.liftIO $ liftedBackgroundColor cfg pct
  let pad = barPadding cfg
      fpad = fromIntegral pad
  C.setSourceRGB bgR bgG bgB
  C.rectangle fpad fpad (fwidth - 2 * fpad) (fheight - 2 * fpad)
  C.fill

  -- Now draw a nice frame
  (frameR, frameG, frameB) <- C.liftIO $ liftedBorderColor cfg
  C.setSourceRGB frameR frameG frameB
  C.setLineWidth 1.0
  C.rectangle (fpad + 0.5) (fpad + 0.5) (fwidth - 2 * fpad - 1) (fheight - 2 * fpad - 1)
  C.stroke

renderBar :: Double -> BarConfig -> Int -> Int -> C.Render ()
renderBar pct cfg width height = do
  let direction = barDirection cfg
      activeHeight = case direction of
                       VERTICAL   -> pct * (fromIntegral height)
                       HORIZONTAL -> fromIntegral height
      activeWidth  = case direction of
                       VERTICAL   -> fromIntegral width
                       HORIZONTAL -> pct * (fromIntegral width)
      newOrigin    = case direction of
                       VERTICAL -> fromIntegral height - activeHeight
                       HORIZONTAL -> 0
      pad = barPadding cfg

  renderFrame pct cfg width height

  -- After we draw the frame, transform the coordinate space so that
  -- we only draw within the frame.
  C.translate (fromIntegral pad + 1) (fromIntegral pad + 1)
  let xS = fromIntegral (width - 2 * pad - 2) / fromIntegral width
      yS = fromIntegral (height - 2 * pad - 2) / fromIntegral height
  C.scale xS yS

  (r, g, b) <- C.liftIO $ liftedBarColor cfg pct
  C.setSourceRGB r g b
  C.translate 0 newOrigin
  C.rectangle 0 0 activeWidth activeHeight
  C.fill

drawBar :: MVar VerticalBarState -> DrawingArea -> IO ()
drawBar mv drawArea = do
  (w, h) <- widgetGetSize drawArea
  drawWin <- widgetGetDrawWindow drawArea
  s <- readMVar mv
  let pct = barPercent s
  modifyMVar_ mv (\s' -> return s' { barIsBootstrapped = True })
  renderWithDrawable drawWin (renderBar pct (barConfig s) w h)

verticalBarNew :: BarConfig -> IO (Widget, VerticalBarHandle)
verticalBarNew cfg = do
  drawArea <- drawingAreaNew

  mv <- newMVar VerticalBarState { barIsBootstrapped = False
                                 , barPercent = 0
                                 , barCanvas = drawArea
                                 , barConfig = cfg
                                 }

  widgetSetSizeRequest drawArea (barWidth cfg) (-1)
  _ <- on drawArea exposeEvent $ tryEvent $ C.liftIO (drawBar mv drawArea)

  box <- hBoxNew False 1
  boxPackStart box drawArea PackGrow 0
  widgetShowAll box

  return (toWidget box, VBH mv)
