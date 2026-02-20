{-# LANGUAGE ScopedTypeVariables #-}

-- | This is a graph widget inspired by the widget of the same name in Awesome
-- (the window manager). It plots a series of data points similarly to a bar
-- graph. This version must be explicitly fed data with 'graphAddSample'. For a
-- more automated version, see "System.Taffybar.Widgets.Generic.PollingGraph".
--
-- Like Awesome, this graph can plot multiple data sets in one widget. The data
-- sets are plotted in the order provided by the caller.
--
-- Note: all of the data fed to this widget should be in the range [0,1].
module System.Taffybar.Widget.Generic.Graph
  ( -- * Types
    GraphHandle,
    GraphConfig (..),
    GraphDirection (..),
    GraphStyle (..),

    -- * Functions
    graphNew,
    graphAddSample,
    defaultGraphConfig,
  )
where

import Control.Concurrent
import Control.Exception (bracket_)
import Control.Monad (forM, when)
import Control.Monad.IO.Class
import Data.Default (Default (..))
import Data.Sequence (Seq (..), ViewL (..), viewl, (<|))
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified GI.Cairo.Render as C
import GI.Cairo.Render.Connector
import qualified GI.Cairo.Render.Matrix as M
import qualified GI.Gdk.Structs.RGBA as GdkRGBA
import qualified GI.Gtk as Gtk
import System.Taffybar.Util
import System.Taffybar.Widget.Util

-- | Handle used to add samples to a graph widget.
newtype GraphHandle = GH (MVar GraphState)

data GraphState
  = GraphState
  { graphIsBootstrapped :: Bool,
    graphHistory :: [Seq Double],
    graphCanvas :: Gtk.DrawingArea,
    graphConfig :: GraphConfig
  }

-- | Direction in which historical samples flow across the widget.
data GraphDirection = LEFT_TO_RIGHT | RIGHT_TO_LEFT deriving (Eq)

-- 'RGBA' represents a color with a transparency.
type GraphColor = (Double, Double, Double, Double)

-- | The style of the graph. Generally, you will want to draw all 'Area' graphs
-- first, and then all 'Line' graphs.
data GraphStyle
  = -- | Thea area below the value is filled
    Area
  | -- | The values are connected by a line (one pixel wide)
    Line

-- | The configuration options for the graph. The padding is the number of
-- pixels reserved as blank space around the widget in each direction.
data GraphConfig = GraphConfig
  { -- | Number of pixels of padding on each side of the graph widget
    graphPadding :: Int,
    -- | The background color of the graph (default black)
    graphBackgroundColor :: GraphColor,
    -- | The border color drawn around the graph (default gray)
    graphBorderColor :: GraphColor,
    -- | The width of the border (default 1, use 0 to disable the border)
    graphBorderWidth :: Int,
    -- | Colors for each data set (default cycles between red, green and blue)
    graphDataColors :: [GraphColor],
    -- | Draw graph colors from CSS classes. When enabled, each dataset color is
    -- read from @color@ using @graph-data-N@ classes on the drawing area.
    graphColorsFromCss :: Bool,
    -- | How to draw each data point (default @repeat Area@)
    graphDataStyles :: [GraphStyle],
    -- | The number of data points to retain for each data set (default 20)
    graphHistorySize :: Int,
    -- | May contain Pango markup (default @Nothing@)
    graphLabel :: Maybe T.Text,
    -- | The width (in pixels) of the graph widget (default 50)
    graphWidth :: Int,
    -- | The direction in which the graph will move as time passes (default LEFT_TO_RIGHT)
    graphDirection :: GraphDirection
  }

-- | Default graph configuration.
defaultGraphConfig :: GraphConfig
defaultGraphConfig =
  GraphConfig
    { graphPadding = 2,
      graphBackgroundColor = (0.0, 0.0, 0.0, 1.0),
      graphBorderColor = (0.5, 0.5, 0.5, 1.0),
      graphBorderWidth = 1,
      graphDataColors = cycle [(1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0)],
      graphColorsFromCss = False,
      graphDataStyles = repeat Area,
      graphHistorySize = 20,
      graphLabel = Nothing,
      graphWidth = 50,
      graphDirection = LEFT_TO_RIGHT
    }

instance Default GraphConfig where
  def = defaultGraphConfig

-- | Add a data point to the graph for each of the tracked data sets. There
-- should be as many values in the list as there are data sets.
graphAddSample :: GraphHandle -> [Double] -> IO ()
graphAddSample (GH mv) rawData = do
  s <- readMVar mv
  let drawArea = graphCanvas s
      histSize = graphHistorySize (graphConfig s)
      histsAndNewVals = zip pcts (graphHistory s)
      newHists = case graphHistory s of
        [] -> map S.singleton pcts
        _ -> map (\(p, h) -> S.take histSize $ p <| h) histsAndNewVals
  when (graphIsBootstrapped s) $ do
    modifyMVar_ mv (\s' -> return s' {graphHistory = newHists})
    postGUIASync $ Gtk.widgetQueueDraw drawArea
  where
    pcts = map (clamp 0 1) rawData

clamp :: Double -> Double -> Double -> Double
clamp lo hi d = max lo $ min hi d

outlineData :: (Double -> Double) -> Double -> Double -> C.Render ()
outlineData pctToY xStep pct = do
  (curX, _) <- C.getCurrentPoint
  C.lineTo (curX + xStep) (pctToY pct)

normalStateFlags :: [Gtk.StateFlags]
normalStateFlags = [Gtk.StateFlagsNormal]

rgbaToGraphColor :: GdkRGBA.RGBA -> IO GraphColor
rgbaToGraphColor rgba = do
  r <- GdkRGBA.getRGBARed rgba
  g <- GdkRGBA.getRGBAGreen rgba
  b <- GdkRGBA.getRGBABlue rgba
  a <- GdkRGBA.getRGBAAlpha rgba
  return (r, g, b, a)

withStyleContextSave :: Gtk.StyleContext -> IO a -> IO a
withStyleContextSave styleContext =
  bracket_
    (Gtk.styleContextSave styleContext)
    (Gtk.styleContextRestore styleContext)

getDataColorFromCss :: Gtk.StyleContext -> Int -> IO GraphColor
getDataColorFromCss styleContext index =
  withStyleContextSave styleContext $ do
    Gtk.styleContextAddClass styleContext (T.pack "graph-data")
    Gtk.styleContextAddClass styleContext (T.pack $ "graph-data-" <> show index)
    Gtk.styleContextGetColor styleContext normalStateFlags >>= rgbaToGraphColor

renderFrameAndBackground :: GraphConfig -> Maybe Gtk.StyleContext -> Int -> Int -> C.Render ()
renderFrameAndBackground cfg styleContext w h = do
  let pad = graphPadding cfg
      fpad = fromIntegral pad
      fw = fromIntegral w
      fh = fromIntegral h
      frameX = fpad
      frameY = fpad
      frameWidth = fw - 2 * fpad
      frameHeight = fh - 2 * fpad
  case (graphColorsFromCss cfg, styleContext) of
    (True, Just context) ->
      toRender $ \cairoCtx -> do
        Gtk.renderBackground context cairoCtx frameX frameY frameWidth frameHeight
        when (graphBorderWidth cfg > 0) $
          Gtk.renderFrame context cairoCtx frameX frameY frameWidth frameHeight
    _ -> do
      let (backR, backG, backB, backA) = graphBackgroundColor cfg
          (frameR, frameG, frameB, frameA) = graphBorderColor cfg
      -- Draw the requested background
      C.setSourceRGBA backR backG backB backA
      C.rectangle frameX frameY frameWidth frameHeight
      C.fill

      -- Draw a frame around the widget area (unless equal to background color,
      -- which likely means the user does not want a frame)
      when (graphBorderWidth cfg > 0) $ do
        let p = fromIntegral (graphBorderWidth cfg)
        C.setLineWidth p
        C.setSourceRGBA frameR frameG frameB frameA
        C.rectangle
          (fpad + (p / 2))
          (fpad + (p / 2))
          (fw - 2 * fpad - p)
          (fh - 2 * fpad - p)
        C.stroke

renderGraph :: [Seq Double] -> [GraphColor] -> GraphConfig -> Maybe Gtk.StyleContext -> Int -> Int -> Double -> C.Render ()
renderGraph hists dataColors cfg styleContext w h xStep = do
  renderFrameAndBackground cfg styleContext w h

  C.setLineWidth 0.1

  let pad = fromIntegral $ graphPadding cfg
  let framePad = fromIntegral $ graphBorderWidth cfg

  -- Make the new origin be inside the frame and then scale the drawing area so
  -- that all operations in terms of width and height are inside the drawn
  -- frame.
  C.translate (pad + framePad) (pad + framePad)
  let xS = (fromIntegral w - 2 * pad - 2 * framePad) / fromIntegral w
      yS = (fromIntegral h - 2 * pad - 2 * framePad) / fromIntegral h
  C.scale xS yS

  -- If right-to-left direction is requested, apply an horizontal inversion
  -- transformation with an offset to the right equal to the width of the
  -- widget.
  when (graphDirection cfg == RIGHT_TO_LEFT) $
    C.transform $
      M.Matrix (-1) 0 0 1 (fromIntegral w) 0

  let pctToY pct = fromIntegral h * (1 - pct)
      renderDataSet hist color style = case viewl hist of
        EmptyL -> return ()
        _oneSample :< Empty -> return ()
        newestSample :< hist' -> do
          let (r, g, b, a) = color
              originY = pctToY newestSample
              originX = 0
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

  sequence_ $
    zipWith3
      renderDataSet
      hists
      dataColors
      (graphDataStyles cfg)

drawBorder :: MVar GraphState -> Gtk.DrawingArea -> C.Render ()
drawBorder mv drawArea = do
  (w, h) <- widgetGetAllocatedSize drawArea
  s <- liftIO $ readMVar mv
  let cfg = graphConfig s
  styleContext <- C.liftIO $ Gtk.widgetGetStyleContext drawArea
  renderFrameAndBackground cfg (Just styleContext) w h
  liftIO $ modifyMVar_ mv (\s' -> return s' {graphIsBootstrapped = True})
  return ()

drawGraph :: MVar GraphState -> Gtk.DrawingArea -> C.Render ()
drawGraph mv drawArea = do
  (w, h) <- widgetGetAllocatedSize drawArea
  drawBorder mv drawArea
  s <- liftIO $ readMVar mv
  let hist = graphHistory s
      cfg = graphConfig s
      histSize = graphHistorySize cfg
  styleContext <- C.liftIO $ Gtk.widgetGetStyleContext drawArea
  dataColors <-
    if graphColorsFromCss cfg
      then C.liftIO $ forM [1 .. length hist] (getDataColorFromCss styleContext)
      else return $ graphDataColors cfg
  let -- Subtract 1 here since the first data point doesn't require
      -- any movement in the X direction
      xStep = fromIntegral w / fromIntegral (histSize - 1)

  case hist of
    [] -> renderFrameAndBackground cfg (Just styleContext) w h
    _ -> renderGraph hist dataColors cfg (Just styleContext) w h xStep

-- | Construct a new graph widget and its mutable handle.
graphNew :: (MonadIO m) => GraphConfig -> m (Gtk.Widget, GraphHandle)
graphNew cfg = liftIO $ do
  drawArea <- Gtk.drawingAreaNew
  _ <- widgetSetClassGI drawArea (T.pack "graph-canvas")
  Gtk.widgetGetStyleContext drawArea >>= \styleContext ->
    Gtk.styleContextAddClass styleContext (T.pack "graph")
  mv <-
    newMVar
      GraphState
        { graphIsBootstrapped = False,
          graphHistory = [],
          graphCanvas = drawArea,
          graphConfig = cfg
        }

  Gtk.widgetSetSizeRequest drawArea (fromIntegral $ graphWidth cfg) (-1)
  _ <- Gtk.onWidgetDraw drawArea $ \ctx ->
    renderWithContext
      (drawGraph mv drawArea)
      ctx
      >> return True
  box <- Gtk.boxNew Gtk.OrientationHorizontal 1
  _ <- widgetSetClassGI box (T.pack "graph")

  Gtk.widgetSetVexpand drawArea True
  Gtk.widgetSetVexpand box True
  Gtk.boxPackStart box drawArea True True 0

  widget <- case graphLabel cfg of
    Nothing -> Gtk.toWidget box
    Just labelText -> do
      overlay <- Gtk.overlayNew
      label <- Gtk.labelNew Nothing
      Gtk.labelSetMarkup label labelText
      Gtk.containerAdd overlay box
      Gtk.overlayAddOverlay overlay label
      Gtk.toWidget overlay

  Gtk.widgetShowAll widget

  return (widget, GH mv)
