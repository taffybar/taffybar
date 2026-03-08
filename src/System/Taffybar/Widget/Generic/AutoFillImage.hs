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

import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import Graphics.UI.GIGtkScalingImage
  ( AutoFillCache (..),
    fitPixbufToBox,
  )
import qualified Graphics.UI.GIGtkScalingImage as Scaling
import System.Taffybar.Widget.Util

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
  void $ widgetSetClassGI drawArea "auto-size-image"
  void $ widgetSetClassGI drawArea "auto-fill-image"
  Scaling.autoFillImage drawArea getPixbuf orientation

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
