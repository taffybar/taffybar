-- | Unified interface for auto-scaling image widgets.
--
-- This module dispatches between the two image scaling implementations
-- ('autoSizeImage' and 'autoFillImage') based on an 'ImageScaleStrategy'.
-- All call sites should use 'scalingImage' (TaffyIO) or 'scalingImageNew'
-- (plain IO with an explicit strategy) instead of calling the underlying
-- implementations directly.
module System.Taffybar.Widget.Generic.ScalingImage
  ( scalingImageNew
  , scalingImage
  , getScalingImageStrategy
  , setScalingImageStrategy
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Int
import           Data.Typeable
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import           StatusNotifier.Tray (scalePixbufToSize)
import           System.Taffybar.Context
import           System.Taffybar.Widget.Generic.AutoFillImage (autoFillImage)
import           System.Taffybar.Widget.Generic.AutoSizeImage
  ( ImageScaleStrategy(..)
  , autoSizeImage
  )

newtype ScalingImageStrategySetting =
  ScalingImageStrategySetting ImageScaleStrategy
  deriving Typeable

-- | Create a scaling image widget using the given strategy.
--
-- Returns a @(widget, refreshAction)@ pair. The widget is a generic
-- 'Gtk.Widget' regardless of which strategy is used.
scalingImageNew
  :: ImageScaleStrategy
  -> (Int32 -> IO (Maybe Gdk.Pixbuf))
  -> Gtk.Orientation
  -> IO (Gtk.Widget, IO ())
scalingImageNew ImageResize getPixbuf orientation = do
  image <- Gtk.imageNew
  let scaledGetter size =
        getPixbuf size >>= traverse (scalePixbufToSize size orientation)
  refresh <- autoSizeImage image scaledGetter orientation
  widget <- Gtk.toWidget image
  return (widget, refresh)
scalingImageNew ImageDraw getPixbuf orientation = do
  drawArea <- Gtk.drawingAreaNew
  refresh <- autoFillImage drawArea getPixbuf orientation
  widget <- Gtk.toWidget drawArea
  return (widget, refresh)

-- | TaffyIO variant that reads the strategy from context state.
scalingImage
  :: (Int32 -> IO (Maybe Gdk.Pixbuf))
  -> Gtk.Orientation
  -> TaffyIO (Gtk.Widget, IO ())
scalingImage getPixbuf orientation = do
  strategy <- getScalingImageStrategy
  liftIO $ scalingImageNew strategy getPixbuf orientation

-- | Read the current global image scale strategy (defaults to 'ImageDraw').
getScalingImageStrategy :: TaffyIO ImageScaleStrategy
getScalingImageStrategy = do
  ScalingImageStrategySetting s <-
    getStateDefault $ return $ ScalingImageStrategySetting ImageDraw
  return s

-- | Set the global image scale strategy in context state.
setScalingImageStrategy :: ImageScaleStrategy -> TaffyIO ()
setScalingImageStrategy strategy =
  void $ setState $ ScalingImageStrategySetting strategy
