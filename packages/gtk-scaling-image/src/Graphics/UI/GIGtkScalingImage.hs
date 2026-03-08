{-# LANGUAGE OverloadedStrings #-}

module Graphics.UI.GIGtkScalingImage
  ( BorderInfo (..),
    borderInfoZero,
    borderWidth,
    borderHeight,
    getBorderInfo,
    getInsetInfo,
    getContentAllocation,
    scalePixbufToSize,
    AutoFillCache (..),
    fitPixbufToBox,
    autoFillImage,
    autoFillImageNew,
  )
where

import qualified Control.Concurrent.MVar as MV
import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import qualified GI.Cairo.Render as C
import GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf.Enums as GdkPixbuf
import GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import System.Log.Logger
import Text.Printf

imageLog :: Priority -> String -> IO ()
imageLog = logM "Graphics.UI.GIGtkScalingImage"

getScaledWidthHeight :: Bool -> Int32 -> Int32 -> Int32 -> (Int32, Int32)
getScaledWidthHeight shouldTargetWidth targetSize width height =
  let getRatio :: Int32 -> Rational
      getRatio toScale =
        fromIntegral targetSize / fromIntegral toScale
      getOther :: Int32 -> Int32 -> Int32
      getOther toScale other = max 1 $ floor $ getRatio toScale * fromIntegral other
   in if shouldTargetWidth
        then (targetSize, getOther width height)
        else (getOther height width, targetSize)

data BorderInfo = BorderInfo
  { borderTop :: Int16,
    borderBottom :: Int16,
    borderLeft :: Int16,
    borderRight :: Int16
  }
  deriving (Eq, Show)

borderInfoZero :: BorderInfo
borderInfoZero = BorderInfo 0 0 0 0

borderWidth :: BorderInfo -> Int16
borderWidth borderInfo = borderLeft borderInfo + borderRight borderInfo

borderHeight :: BorderInfo -> Int16
borderHeight borderInfo = borderTop borderInfo + borderBottom borderInfo

borderFunctions :: [Gtk.StyleContext -> [Gtk.StateFlags] -> IO Gtk.Border]
borderFunctions =
  [ Gtk.styleContextGetPadding,
    Gtk.styleContextGetMargin,
    Gtk.styleContextGetBorder
  ]

insetFunctions :: [Gtk.StyleContext -> [Gtk.StateFlags] -> IO Gtk.Border]
insetFunctions =
  [ Gtk.styleContextGetPadding,
    Gtk.styleContextGetBorder
  ]

toBorderInfo :: (MonadIO m) => Gtk.Border -> m BorderInfo
toBorderInfo border =
  BorderInfo
    <$> Gtk.getBorderTop border
    <*> Gtk.getBorderBottom border
    <*> Gtk.getBorderLeft border
    <*> Gtk.getBorderRight border

addBorderInfo :: BorderInfo -> BorderInfo -> BorderInfo
addBorderInfo
  (BorderInfo t1 b1 l1 r1)
  (BorderInfo t2 b2 l2 r2) =
    BorderInfo (t1 + t2) (b1 + b2) (l1 + l2) (r1 + r2)

getBorderInfo :: (MonadIO m, Gtk.IsWidget a) => a -> m BorderInfo
getBorderInfo widget = liftIO $ do
  stateFlags <- Gtk.widgetGetStateFlags widget
  styleContext <- Gtk.widgetGetStyleContext widget
  let getBorderInfoFor borderFn =
        borderFn styleContext stateFlags >>= toBorderInfo
      combineBorderInfo lastSum fn =
        addBorderInfo lastSum <$> getBorderInfoFor fn
  foldM combineBorderInfo borderInfoZero borderFunctions

getInsetInfo :: (MonadIO m, Gtk.IsWidget a) => a -> m BorderInfo
getInsetInfo widget = liftIO $ do
  stateFlags <- Gtk.widgetGetStateFlags widget
  styleContext <- Gtk.widgetGetStyleContext widget
  let getBorderInfoFor borderFn =
        borderFn styleContext stateFlags >>= toBorderInfo
      combineBorderInfo lastSum fn =
        addBorderInfo lastSum <$> getBorderInfoFor fn
  foldM combineBorderInfo borderInfoZero insetFunctions

getContentAllocation ::
  (MonadIO m, Gtk.IsWidget a) =>
  a ->
  BorderInfo ->
  m Gdk.Rectangle
getContentAllocation widget borderInfo = do
  allocation <- Gtk.widgetGetAllocation widget
  currentWidth <- Gdk.getRectangleWidth allocation
  currentHeight <- Gdk.getRectangleHeight allocation
  currentX <- Gdk.getRectangleX allocation
  currentY <- Gdk.getRectangleY allocation

  Gdk.setRectangleWidth allocation $
    max 1 $
      currentWidth - fromIntegral (borderWidth borderInfo)
  Gdk.setRectangleHeight allocation $
    max 1 $
      currentHeight - fromIntegral (borderHeight borderInfo)
  Gdk.setRectangleX allocation $
    currentX + fromIntegral (borderLeft borderInfo)
  Gdk.setRectangleY allocation $
    currentY + fromIntegral (borderTop borderInfo)

  pure allocation

scalePixbufToSize :: Int32 -> Gtk.Orientation -> Gdk.Pixbuf -> IO Gdk.Pixbuf
scalePixbufToSize size orientation pixbuf = do
  width <- Gdk.pixbufGetWidth pixbuf
  height <- Gdk.pixbufGetHeight pixbuf
  let warnAndReturnOrig =
        imageLog WARNING "Unable to scale pixbuf" >> pure pixbuf
  if size <= 0
    then pure pixbuf
    else
      if width <= 0 || height <= 0
        then warnAndReturnOrig
        else do
          let targetWidth = case orientation of
                Gtk.OrientationHorizontal -> False
                _ -> True
              (scaledWidth, scaledHeight) =
                getScaledWidthHeight targetWidth size width height
          imageLog DEBUG $
            printf
              "Scaling pb to %s, actualW: %s, actualH: %s, scaledW: %s, scaledH: %s"
              (show size)
              (show width)
              (show height)
              (show scaledWidth)
              (show scaledHeight)
          maybe
            warnAndReturnOrig
            pure
            =<< Gdk.pixbufScaleSimple pixbuf scaledWidth scaledHeight GdkPixbuf.InterpTypeBilinear

data AutoFillCache = AutoFillCache
  { afRequestSize :: Int32,
    afScaleFactor :: Int32,
    afInsets :: BorderInfo,
    afContentWidth :: Int32,
    afContentHeight :: Int32,
    afSourcePixbuf :: Maybe Gdk.Pixbuf,
    afScaledPixbuf :: Maybe Gdk.Pixbuf,
    afOffsetX :: Double,
    afOffsetY :: Double
  }

fitPixbufToBox ::
  Int32 ->
  BorderInfo ->
  Int32 ->
  Int32 ->
  Gdk.Pixbuf ->
  IO (Int32, Int32, Double, Double, Maybe Gdk.Pixbuf)
fitPixbufToBox scaleFactor insets allocW allocH pixbuf = do
  pbW' <- Gdk.getPixbufWidth pixbuf
  pbH' <- Gdk.getPixbufHeight pixbuf

  let contentW = max 1 $ allocW - fromIntegral (borderWidth insets)
      contentH = max 1 $ allocH - fromIntegral (borderHeight insets)
      targetWDev = max 1 $ contentW * scaleFactor
      targetHDev = max 1 $ contentH * scaleFactor
      pbW = fromIntegral pbW' :: Double
      pbH = fromIntegral pbH' :: Double
      targetW = fromIntegral targetWDev :: Double
      targetH = fromIntegral targetHDev :: Double
      scale =
        if pbW <= 0 || pbH <= 0
          then 1
          else min (targetW / pbW) (targetH / pbH)
      drawWDev = max 1 $ floor (pbW * scale)
      drawHDev = max 1 $ floor (pbH * scale)
      drawWLogical = fromIntegral drawWDev / fromIntegral scaleFactor
      drawHLogical = fromIntegral drawHDev / fromIntegral scaleFactor
      leftInset = fromIntegral (borderLeft insets) :: Double
      topInset = fromIntegral (borderTop insets) :: Double
      offsetX = leftInset + (fromIntegral contentW - drawWLogical) / 2
      offsetY = topInset + (fromIntegral contentH - drawHLogical) / 2

  scaledM <- Gdk.pixbufScaleSimple pixbuf drawWDev drawHDev GdkPixbuf.InterpTypeBilinear
  pure (contentW, contentH, offsetX, offsetY, scaledM)

autoFillImage ::
  (MonadIO m) =>
  Gtk.DrawingArea ->
  (Int32 -> IO (Maybe Gdk.Pixbuf)) ->
  Gtk.Orientation ->
  m (IO ())
autoFillImage drawArea getPixbuf orientation = liftIO $ do
  case orientation of
    Gtk.OrientationHorizontal -> Gtk.widgetSetVexpand drawArea True
    _ -> Gtk.widgetSetHexpand drawArea True

  Gtk.widgetSetSizeRequest drawArea 16 16

  cacheVar <-
    MV.newMVar
      AutoFillCache
        { afRequestSize = 0,
          afScaleFactor = 1,
          afInsets = borderInfoZero,
          afContentWidth = 1,
          afContentHeight = 1,
          afSourcePixbuf = Nothing,
          afScaledPixbuf = Nothing,
          afOffsetX = 0,
          afOffsetY = 0
        }

  let recompute force = do
        allocation <- Gtk.widgetGetAllocation drawArea
        allocW <- Gdk.getRectangleWidth allocation
        allocH <- Gdk.getRectangleHeight allocation
        insets <- getInsetInfo drawArea
        scaleFactor <- Gtk.widgetGetScaleFactor drawArea

        let contentW = max 1 $ allocW - fromIntegral (borderWidth insets)
            contentH = max 1 $ allocH - fromIntegral (borderHeight insets)
            requestSize =
              case orientation of
                Gtk.OrientationHorizontal -> contentH
                _ -> contentW
            scaledFactor = max 1 scaleFactor

        Gtk.widgetSetSizeRequest
          drawArea
          (fromIntegral requestSize + fromIntegral (borderWidth insets))
          (fromIntegral requestSize + fromIntegral (borderHeight insets))

        old <- MV.readMVar cacheVar
        srcFresh <-
          if force || requestSize /= afRequestSize old
            then getPixbuf requestSize
            else pure Nothing

        let src = maybe (afSourcePixbuf old) Just srcFresh
            needsRefit =
              force
                || requestSize /= afRequestSize old
                || scaledFactor /= afScaleFactor old
                || insets /= afInsets old
                || contentW /= afContentWidth old
                || contentH /= afContentHeight old

        when needsRefit $ do
          newCache <-
            case src of
              Nothing ->
                pure
                  old
                    { afRequestSize = requestSize,
                      afScaleFactor = scaledFactor,
                      afInsets = insets,
                      afContentWidth = contentW,
                      afContentHeight = contentH,
                      afSourcePixbuf = Nothing,
                      afScaledPixbuf = Nothing,
                      afOffsetX = 0,
                      afOffsetY = 0
                    }
              Just pb -> do
                (cw, ch, ox, oy, scaledM) <-
                  fitPixbufToBox scaledFactor insets allocW allocH pb
                pure
                  old
                    { afRequestSize = requestSize,
                      afScaleFactor = scaledFactor,
                      afInsets = insets,
                      afContentWidth = cw,
                      afContentHeight = ch,
                      afSourcePixbuf = Just pb,
                      afScaledPixbuf = scaledM,
                      afOffsetX = ox,
                      afOffsetY = oy
                    }

          void $ MV.swapMVar cacheVar newCache
          Gtk.widgetQueueDraw drawArea

  void $ Gtk.onWidgetSizeAllocate drawArea $ \_ -> recompute False
  void $ Gtk.onWidgetStyleUpdated drawArea $ recompute True

  void $ Gtk.onWidgetDraw drawArea $ \ctx -> do
    st <- MV.readMVar cacheVar
    case afScaledPixbuf st of
      Nothing -> pure True
      Just pb -> do
        Gdk.cairoSetSourcePixbuf ctx pb (afOffsetX st) (afOffsetY st)
        renderWithContext C.paint ctx
        pure True

  pure $ recompute True

autoFillImageNew ::
  (MonadIO m) =>
  (Int32 -> IO (Maybe Gdk.Pixbuf)) ->
  Gtk.Orientation ->
  m Gtk.DrawingArea
autoFillImageNew getPixbuf orientation = do
  drawArea <- Gtk.drawingAreaNew
  void $ autoFillImage drawArea getPixbuf orientation
  pure drawArea
