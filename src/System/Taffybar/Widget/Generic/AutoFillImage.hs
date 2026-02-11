{-# LANGUAGE OverloadedStrings #-}

-- | A draw-based alternative to "System.Taffybar.Widget.Generic.AutoSizeImage"
-- that scales a pixbuf to fit its allocated area while preserving aspect ratio,
-- avoiding the resize feedback loops inherent in 'Gtk.Image'.
module System.Taffybar.Widget.Generic.AutoFillImage
  ( autoFillImage,
    autoFillImageNew,
    AutoFillCache (..),
    fitPixbufToBox,
  )
where

import qualified Control.Concurrent.MVar as MV
import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import qualified GI.Cairo.Render as C
import GI.Cairo.Render.Connector
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf.Enums as GdkPixbuf
import GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import System.Taffybar.Widget.Generic.AutoSizeImage
import System.Taffybar.Widget.Util

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
  -- | scale factor
  Int32 ->
  BorderInfo ->
  -- | allocated width (logical px)
  Int32 ->
  -- | allocated height (logical px)
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
  -- GDK can return NULL; treat that as "draw nothing".
  pure (contentW, contentH, offsetX, offsetY, scaledM)

-- | A draw-based alternative to 'autoSizeImage' that avoids resize loops and
-- naturally responds to CSS changes. The widget will always draw the current
-- pixbuf scaled to fit its allocated area (minus padding+border).
--
-- This uses a 'Gtk.DrawingArea' instead of 'Gtk.Image' because GTK3 does not
-- support "scale-to-allocation" semantics for 'Gtk.Image'.
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

  -- Keep existing styling working.
  void $ widgetSetClassGI drawArea "auto-size-image"
  void $ widgetSetClassGI drawArea "auto-fill-image"

  -- Ensure the widget has a non-zero natural size even before the first
  -- allocation.
  Gtk.widgetSetSizeRequest drawArea 16 16

  -- Cache is only accessed from the GTK main loop via signal handlers.
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

        -- CSS can change dynamically (taffybar supports live CSS reload), so we
        -- recompute insets every time we recompute sizing.
        insets <- getInsetInfo drawArea
        scaleFactor <- Gtk.widgetGetScaleFactor drawArea

        let contentW = max 1 $ allocW - fromIntegral (borderWidth insets)
            contentH = max 1 $ allocH - fromIntegral (borderHeight insets)
            requestSize =
              case orientation of
                Gtk.OrientationHorizontal -> contentH
                _ -> contentW

        -- Update the widget's natural size so it won't collapse to 0 when packed
        -- without expand.
        Gtk.widgetSetSizeRequest
          drawArea
          (fromIntegral requestSize + fromIntegral (borderWidth insets))
          (fromIntegral requestSize + fromIntegral (borderHeight insets))

        old <- MV.readMVar cacheVar
        srcFresh <-
          if force || requestSize /= afRequestSize old
            then getPixbuf requestSize
            else pure Nothing

        -- If the getter fails transiently, keep drawing the last known pixbuf.
        let src =
              case srcFresh of
                Just pb -> Just pb
                Nothing -> afSourcePixbuf old

        let needsRefit =
              force
                || requestSize /= afRequestSize old
                || scaleFactor /= afScaleFactor old
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
                      afScaleFactor = scaleFactor,
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
                  fitPixbufToBox (max 1 scaleFactor) insets allocW allocH pb
                pure
                  old
                    { afRequestSize = requestSize,
                      afScaleFactor = max 1 scaleFactor,
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

  -- Redraw when GTK allocates or when style changes.
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

-- | Convenience constructor for 'autoFillImage'.
autoFillImageNew ::
  (MonadIO m) =>
  (Int32 -> IO (Maybe Gdk.Pixbuf)) ->
  Gtk.Orientation ->
  m Gtk.DrawingArea
autoFillImageNew getPixBuf orientation = do
  drawArea <- Gtk.drawingAreaNew
  void $ autoFillImage drawArea getPixBuf orientation
  pure drawArea
