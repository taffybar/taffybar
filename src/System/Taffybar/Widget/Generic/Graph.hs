{-# LANGUAGE ScopedTypeVariables #-}
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
module System.Taffybar.Widget.Generic.Graph (
  -- * Types
    GraphHandle
  , GraphConfig(..)
  , GraphDirection(..)
  , GraphStyle(..)
  -- * Functions
  , graphNew
  , graphAddSample
  , defaultGraphConfig
  ) where

import           Control.Concurrent
import           Control.Monad ( when )
import           Control.Monad.IO.Class
import           Data.Foldable ( mapM_ )
import           Data.Sequence ( Seq, (<|), viewl, ViewL(..) )
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified GI.Cairo.Render as C
import           GI.Cairo.Render.Connector
import qualified GI.Cairo.Render.Matrix as M
import qualified GI.Gtk as Gtk
import           Prelude hiding ( mapM_ )
import           System.Taffybar.Util
import           System.Taffybar.Widget.Util

newtype GraphHandle = GH (MVar GraphState)
data GraphState =
  GraphState { graphIsBootstrapped :: Bool
             , graphHistory :: [Seq Double]
             , graphCanvas :: Gtk.DrawingArea
             , graphConfig :: GraphConfig
             }

data GraphDirection = LEFT_TO_RIGHT | RIGHT_TO_LEFT deriving (Eq)

type RGBA = (Double, Double, Double, Double)

-- | The style of the graph. Generally, you will want to draw all 'Area' graphs first, and then all 'Line' graphs.
data GraphStyle
    = Area -- ^ Thea area below the value is filled
    | Line -- ^ The values are connected by a line (one pixel wide)

-- | The configuration options for the graph.  The padding is the
-- number of pixels reserved as blank space around the widget in each
-- direction.
data GraphConfig = GraphConfig {
  -- | Number of pixels of padding on each side of the graph widget
    graphPadding :: Int
  -- | The background color of the graph (default black)
  , graphBackgroundColor :: RGBA
  -- | The border color drawn around the graph (default gray)
  , graphBorderColor :: RGBA
  -- | The width of the border (default 1, use 0 to disable the border)
  , graphBorderWidth :: Int
  -- | Colors for each data set (default cycles between red, green and blue)
  , graphDataColors :: [RGBA]
  -- | How to draw each data point (default @repeat Area@)
  , graphDataStyles :: [GraphStyle]
  -- | The number of data points to retain for each data set (default 20)
  , graphHistorySize :: Int
  -- | May contain Pango markup (default @Nothing@)
  , graphLabel :: Maybe T.Text
  -- | The width (in pixels) of the graph widget (default 50)
  , graphWidth :: Int
  -- | The direction in which the graph will move as time passes (default LEFT_TO_RIGHT)
  , graphDirection :: GraphDirection
  }

defaultGraphConfig :: GraphConfig
defaultGraphConfig =
  GraphConfig
  { graphPadding = 2
  , graphBackgroundColor = (0.0, 0.0, 0.0, 1.0)
  , graphBorderColor = (0.5, 0.5, 0.5, 1.0)
  , graphBorderWidth = 1
  , graphDataColors = cycle [(1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0)]
  , graphDataStyles = repeat Area
  , graphHistorySize = 20
  , graphLabel = Nothing
  , graphWidth = 50
  , graphDirection = LEFT_TO_RIGHT
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
  when (graphIsBootstrapped s) $ do
    modifyMVar_ mv (\s' -> return s' { graphHistory = newHists })
    postGUIASync $ Gtk.widgetQueueDraw drawArea
  where
    pcts = map (clamp 0 1) rawData

clamp :: Double -> Double -> Double -> Double
clamp lo hi d = max lo $ min hi d

outlineData :: (Double -> Double) -> Double -> Double -> C.Render ()
outlineData pctToY xStep pct = do
  (curX,_) <- C.getCurrentPoint
  C.lineTo (curX + xStep) (pctToY pct)

renderFrameAndBackground :: GraphConfig -> Int -> Int -> C.Render ()
renderFrameAndBackground cfg w h = do
  let (backR, backG, backB, backA) = graphBackgroundColor cfg
      (frameR, frameG, frameB, frameA) = graphBorderColor cfg
      pad = graphPadding cfg
      fpad = fromIntegral pad
      fw = fromIntegral w
      fh = fromIntegral h

  -- Draw the requested background
  C.setSourceRGBA backR backG backB backA
  C.rectangle fpad fpad (fw - 2 * fpad) (fh - 2 * fpad)
  C.fill

  -- Draw a frame around the widget area
  -- (unless equal to background color, which likely means the user does not
  -- want a frame)
  when (graphBorderWidth cfg > 0) $ do
    let p = fromIntegral (graphBorderWidth cfg)
    C.setLineWidth p
    C.setSourceRGBA frameR frameG frameB frameA
    C.rectangle (fpad + (p / 2)) (fpad + (p / 2)) (fw - 2 * fpad - p) (fh - 2 * fpad - p)
    C.stroke


renderGraph :: [Seq Double] -> GraphConfig -> Int -> Int -> Double -> C.Render ()
renderGraph hists cfg w h xStep = do
  renderFrameAndBackground cfg w h

  C.setLineWidth 0.1

  let pad = fromIntegral $ graphPadding cfg
  let framePad = fromIntegral $ graphBorderWidth cfg

  -- Make the new origin be inside the frame and then scale the
  -- drawing area so that all operations in terms of width and height
  -- are inside the drawn frame.
  C.translate (pad + framePad) (pad + framePad)
  let xS = (fromIntegral w - 2 * pad - 2 * framePad) / fromIntegral w
      yS = (fromIntegral h - 2 * pad - 2 * framePad) / fromIntegral h
  C.scale xS yS

  -- If right-to-left direction is requested, apply an horizontal inversion
  -- transformation with an offset to the right equal to the width of the widget.
  when (graphDirection cfg == RIGHT_TO_LEFT) $
      C.transform $ M.Matrix (-1) 0 0 1 (fromIntegral w) 0

  let pctToY pct = fromIntegral h * (1 - pct)
      renderDataSet hist color style
        | S.length hist <= 1 = return ()
        | otherwise = do
          let (r, g, b, a) = color
              originY = pctToY newestSample
              originX = 0
              newestSample :< hist' = viewl hist
          C.setSourceRGBA r g b a
          C.moveTo originX originY

          mapM_ (outlineData pctToY xStep) hist'
          case style of
            Area -> do
              (endX, _) <- C.getCurrentPoint
              C.lineTo endX (fromIntegral h)
              C.lineTo 0 (fromIntegral h)
              C.fill
            Line -> do
              C.setLineWidth 1.0
              C.stroke


  sequence_ $ zipWith3 renderDataSet hists (graphDataColors cfg) (graphDataStyles cfg)

drawBorder :: MVar GraphState -> Gtk.DrawingArea -> C.Render ()
drawBorder mv drawArea = do
  (w, h) <- widgetGetAllocatedSize drawArea
  s <- liftIO $ readMVar mv
  let cfg = graphConfig s
  renderFrameAndBackground cfg w h
  liftIO $ modifyMVar_ mv (\s' -> return s' { graphIsBootstrapped = True })
  return ()

drawGraph :: MVar GraphState -> Gtk.DrawingArea ->  C.Render ()
drawGraph mv drawArea = do
  (w, h) <- widgetGetAllocatedSize drawArea
  drawBorder mv drawArea
  s <- liftIO $ readMVar mv
  let hist = graphHistory s
      cfg = graphConfig s
      histSize = graphHistorySize cfg
      -- Subtract 1 here since the first data point doesn't require
      -- any movement in the X direction
      xStep = fromIntegral w / fromIntegral (histSize - 1)

  case hist of
    [] -> renderFrameAndBackground cfg w h
    _ -> renderGraph hist cfg w h xStep

graphNew :: MonadIO m => GraphConfig -> m (Gtk.Widget, GraphHandle)
graphNew cfg = liftIO $ do
  drawArea <- Gtk.drawingAreaNew
  mv <- newMVar GraphState { graphIsBootstrapped = False
                           , graphHistory = []
                           , graphCanvas = drawArea
                           , graphConfig = cfg
                           }

  Gtk.widgetSetSizeRequest drawArea (fromIntegral $ graphWidth cfg) (-1)
  _ <- Gtk.onWidgetDraw drawArea (\ctx -> renderWithContext (drawGraph mv drawArea) ctx >> return True)
  box <- Gtk.boxNew Gtk.OrientationHorizontal 1

  case graphLabel cfg of
    Nothing  -> return ()
    Just lbl -> do
      l <- Gtk.labelNew (Nothing :: Maybe T.Text)
      Gtk.labelSetMarkup l lbl
      Gtk.boxPackStart box l False False 0

  Gtk.widgetSetVexpand drawArea True
  Gtk.widgetSetVexpand box True
  Gtk.boxPackStart box drawArea True True 0
  Gtk.widgetShowAll box
  giBox <- Gtk.toWidget box
  return (giBox, GH mv)
