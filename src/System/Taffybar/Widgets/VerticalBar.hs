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
  defaultBarConfig
  ) where

import Control.Concurrent
import Graphics.Rendering.Cairo
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
            , barBackgroundColor :: (Double, Double, Double) -- ^ The background color of the widget
            , barColor :: Double -> (Double, Double, Double) -- ^ A function to determine the color of the widget for the current data point
            , barPadding :: Int -- ^ Number of pixels of padding around the widget
            , barWidth :: Int
            , barDirection :: BarDirection
            }

-- | A default bar configuration.  The color of the active portion of
-- the bar must be specified.
defaultBarConfig :: (Double -> (Double, Double, Double)) -> BarConfig
defaultBarConfig c = BarConfig { barBorderColor = (0.5, 0.5, 0.5)
                               , barBackgroundColor = (0, 0, 0)
                               , barColor = c
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

renderFrame :: BarConfig -> Int -> Int -> Render ()
renderFrame cfg width height = do
  let fwidth = fromIntegral width
      fheight = fromIntegral height

  -- Now draw the user's requested background, respecting padding
  let (bgR, bgG, bgB) = barBackgroundColor cfg
      pad = barPadding cfg
      fpad = fromIntegral pad
  setSourceRGB bgR bgG bgB
  rectangle fpad fpad (fwidth - 2 * fpad) (fheight - 2 * fpad)
  fill

  -- Now draw a nice frame
  let (frameR, frameG, frameB) = barBorderColor cfg
  setSourceRGB frameR frameG frameB
  setLineWidth 1.0
  rectangle fpad fpad (fwidth - 2 * fpad) (fheight - 2 * fpad)
  stroke

-- renderBar :: Double -> (Double, Double, Double) -> Int -> Int -> Render ()
renderBar :: Double -> BarConfig -> Int -> Int -> Render ()
renderBar pct cfg width height = do
-- renderBar pct (r, g, b) width height = do
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

  renderFrame cfg width height

  -- After we draw the frame, transform the coordinate space so that
  -- we only draw within the frame.
  translate (fromIntegral pad + 1) (fromIntegral pad + 1)
  let xS = fromIntegral (width - 2 * pad - 2) / fromIntegral width
      yS = fromIntegral (height - 2 * pad - 2) / fromIntegral height
  scale xS yS

  let (r, g, b) = (barColor cfg) pct
  setSourceRGB r g b
  translate 0 newOrigin
  rectangle 0 0 activeWidth activeHeight
  fill

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
  _ <- on drawArea exposeEvent $ tryEvent $ liftIO (drawBar mv drawArea)

  box <- hBoxNew False 1
  boxPackStart box drawArea PackGrow 0
  widgetShowAll box

  return (toWidget box, VBH mv)
