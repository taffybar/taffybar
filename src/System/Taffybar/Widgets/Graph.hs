-- | This is a graph widget inspired by the widget of the same name in
-- Awesome (the window manager).  It plots a series of data points
-- similarly to a bar graph.  This version must be explicitly fed data
-- with 'graphAddSample'.  For a more automated version, see
-- 'PollingGraph'.
--
-- Like Awesome, this graph can plot multiple data sets in one widget.
-- The data sets are plotted in the order provided by the caller.
--
-- Note: all of the data fed to this widget should be in the range
-- [0,1].
module System.Taffybar.Widgets.Graph (
  -- * Types
  GraphHandle,
  GraphConfig(..),
  -- * Functions
  graphNew,
  graphAddSample,
  defaultGraphConfig
  ) where

import Prelude hiding ( mapM_ )
import Control.Concurrent
import Data.Sequence ( Seq, (<|), viewl, ViewL(..) )
import Data.Foldable ( mapM_ )
import qualified Data.Sequence as S
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk

newtype GraphHandle = GH (MVar GraphState)
data GraphState =
  GraphState { graphIsBootstrapped :: Bool
             , graphHistory :: [Seq Double]
             , graphCanvas :: DrawingArea
             , graphConfig :: GraphConfig
             }

-- | The configuration options for the graph.  The padding is the
-- number of pixels reserved as blank space around the widget in each
-- direction.
data GraphConfig =
  GraphConfig { graphPadding :: Int -- ^ Number of pixels of padding on each side of the graph widget
              , graphBackgroundColor :: (Double, Double, Double) -- ^ The background color of the graph (default black)
              , graphBorderColor :: (Double, Double, Double) -- ^ The border color drawn around the graph (default gray)
              , graphDataColors :: [(Double, Double, Double, Double)] -- ^ Colors for each data set (default [])
              , graphHistorySize :: Int -- ^ The number of data points to retain for each data set (default 20)
              , graphLabel :: Maybe String -- ^ May contain Pango markup (default Nothing)
              , graphWidth :: Int -- ^ The width (in pixels) of the graph widget (default 50)
              }

defaultGraphConfig :: GraphConfig
defaultGraphConfig = GraphConfig { graphPadding = 2
                                 , graphBackgroundColor = (0.0, 0.0, 0.0)
                                 , graphBorderColor = (0.5, 0.5, 0.5)
                                 , graphDataColors = []
                                 , graphHistorySize = 20
                                 , graphLabel = Nothing
                                 , graphWidth = 50
                                 }

-- | Add a data point to the graph for each of the tracked data sets.
-- There should be as many values in the list as there are data sets.
graphAddSample :: GraphHandle -> [Double] -> IO ()
graphAddSample (GH mv) rawData = do
  s <- readMVar mv
  let drawArea = graphCanvas s
      histSize = graphHistorySize (graphConfig s)
      histsAndNewVals = zip pcts (graphHistory s)
      newHists = case graphHistory s of
        [] -> map S.singleton pcts
        _ -> map (\(p,h) -> S.take histSize $ p <| h) histsAndNewVals
  case graphIsBootstrapped s of
    False -> return ()
    True -> do
      modifyMVar_ mv (\s' -> return s' { graphHistory = newHists })
      postGUIAsync $ widgetQueueDraw drawArea
  where
    pcts = map (clamp 0 1) rawData

clamp :: Double -> Double -> Double -> Double
clamp lo hi d = max lo $ min hi d

outlineData :: (Double -> Double) -> Double -> Double -> Render ()
outlineData pctToY xStep pct = do
  (curX,_) <- getCurrentPoint
  lineTo (curX + xStep) (pctToY pct)

renderFrameAndBackground :: GraphConfig -> Int -> Int -> Render ()
renderFrameAndBackground cfg w h = do
  let (backR, backG, backB) = graphBackgroundColor cfg
      (frameR, frameG, frameB) = graphBorderColor cfg
      pad = graphPadding cfg
      fpad = fromIntegral pad
      fw = fromIntegral w
      fh = fromIntegral h

  -- Clear the background to match the bar
  setSourceRGB 0 0 0
  rectangle 0 0 fw fh
  fill

  -- Draw the requested background
  setSourceRGB backR backG backB
  rectangle fpad fpad (fw - 2 * fpad) (fh - 2 * fpad)
  fill

  -- Draw a frame around the widget area
  setLineWidth 1.0
  setSourceRGB frameR frameG frameB
  rectangle fpad fpad (fw - 2 * fpad) (fh - 2 * fpad)
  stroke


renderGraph :: [Seq Double] -> GraphConfig -> Int -> Int -> Double -> Render ()
renderGraph hists cfg w h xStep = do
  renderFrameAndBackground cfg w h

  setLineWidth 0.1


  let pad = graphPadding cfg

  -- Make the new origin be inside the frame and then scale the
  -- drawing area so that all operations in terms of width and height
  -- are inside the drawn frame.
  translate (fromIntegral pad + 1) (fromIntegral pad + 1)
  let xS = fromIntegral (w - 2 * pad - 2) / fromIntegral w
      yS = fromIntegral (h - 2 * pad - 2) / fromIntegral h
  scale xS yS

  let pctToY pct = fromIntegral h * (1 - pct)
      histsAndColors = zip hists (graphDataColors cfg)
      renderDataSet (hist, color)
        | S.length hist <= 1 = return ()
        | otherwise = do
          let (r, g, b, a) = color
              originY = pctToY newestSample
              originX = 0
              newestSample :< hist' = viewl hist
          setSourceRGBA r g b a
          moveTo originX originY

          mapM_ (outlineData pctToY xStep) hist'
          (endX, _) <- getCurrentPoint
          lineTo endX (fromIntegral h)
          lineTo 0 (fromIntegral h)
          fill


  mapM_ renderDataSet histsAndColors

drawBorder :: MVar GraphState -> DrawingArea -> IO ()
drawBorder mv drawArea = do
  (w, h) <- widgetGetSize drawArea
  drawWin <- widgetGetDrawWindow drawArea
  s <- readMVar mv
  let cfg = graphConfig s
  renderWithDrawable drawWin (renderFrameAndBackground cfg w h)
  modifyMVar_ mv (\s' -> return s' { graphIsBootstrapped = True })
  return ()

drawGraph :: MVar GraphState -> DrawingArea -> IO ()
drawGraph mv drawArea = do
  (w, h) <- widgetGetSize drawArea
  drawWin <- widgetGetDrawWindow drawArea
  s <- readMVar mv
  let hist = graphHistory s
      cfg = graphConfig s
      histSize = graphHistorySize cfg
      -- Subtract 1 here since the first data point doesn't require
      -- any movement in the X direction
      xStep = fromIntegral w / fromIntegral (histSize - 1)

  case hist of
    [] -> renderWithDrawable drawWin (renderFrameAndBackground cfg w h)
    _ -> renderWithDrawable drawWin (renderGraph hist cfg w h xStep)

graphNew :: GraphConfig -> IO (Widget, GraphHandle)
graphNew cfg = do
  drawArea <- drawingAreaNew
  mv <- newMVar GraphState { graphIsBootstrapped = False
                           , graphHistory = []
                           , graphCanvas = drawArea
                           , graphConfig = cfg
                           }

  widgetSetSizeRequest drawArea (graphWidth cfg) (-1)
  _ <- on drawArea exposeEvent $ tryEvent $ liftIO (drawGraph mv drawArea)
  _ <- on drawArea realize $ liftIO (drawBorder mv drawArea)

  case graphLabel cfg of
    Nothing -> do
      widgetShowAll drawArea
      return (toWidget drawArea, GH mv)
    Just lbl -> do
      l <- labelNew Nothing
      box <- hBoxNew False 1
      labelSetMarkup l lbl
      boxPackStart box l PackNatural 0
      boxPackStart box drawArea PackGrow 0

      widgetShowAll box

      return (toWidget box, GH mv)
