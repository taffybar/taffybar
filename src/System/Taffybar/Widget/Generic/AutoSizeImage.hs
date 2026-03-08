{-# LANGUAGE OverloadedStrings #-}

-- | Auto-scaling image helpers that resize pixbufs to widget allocation.
module System.Taffybar.Widget.Generic.AutoSizeImage
  ( autoSizeImage,
    autoSizeImageNew,
    imageMenuItemNew,
    ImageScaleStrategy (..),
    BorderInfo (..),
    borderInfoZero,
    borderWidth,
    borderHeight,
    getBorderInfo,
    getInsetInfo,
    getContentAllocation,
  )
where

import qualified Control.Concurrent.MVar as MV
import Control.Monad
import Control.Monad.IO.Class
import Data.Default (Default (..))
import Data.Int
import Data.Maybe
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import Graphics.UI.GIGtkScalingImage
  ( BorderInfo (..),
    borderHeight,
    borderInfoZero,
    borderWidth,
    getBorderInfo,
    getContentAllocation,
    getInsetInfo,
    scalePixbufToSize,
  )
import System.Log.Logger
import System.Taffybar.Util
import System.Taffybar.Widget.Util
import Text.Printf

-- | Strategy for how auto-scaling image widgets render their pixbufs.
data ImageScaleStrategy
  = -- | Use 'Gtk.Image' with 'imageSetFromPixbuf' + 'queueResize' (original behavior).
    ImageResize
  | -- | Use 'Gtk.DrawingArea' with Cairo rendering (avoids resize feedback loops).
    ImageDraw
  deriving (Eq, Show)

instance Default ImageScaleStrategy where
  def = ImageDraw

imageLog :: Priority -> String -> IO ()
imageLog = logM "System.Taffybar.Widget.Generic.AutoSizeImage"

-- | Automatically update the "Gdk.Pixbuf" of a "Gtk.Image" using the provided
-- action whenever the "Gtk.Image" is allocated. Returns an action that forces a
-- refresh of the image through the provided action.
autoSizeImage ::
  (MonadIO m) =>
  Gtk.Image ->
  (Int32 -> IO (Maybe Gdk.Pixbuf)) ->
  Gtk.Orientation ->
  m (IO ())
autoSizeImage image getPixbuf orientation = liftIO $ do
  case orientation of
    Gtk.OrientationHorizontal -> Gtk.widgetSetVexpand image True
    _ -> Gtk.widgetSetHexpand image True

  _ <- widgetSetClassGI image "auto-size-image"

  lastAllocation <- MV.newMVar 0
  -- XXX: Gtk seems to report information about padding etc inconsistently,
  -- which is why we look it up once, at startup. This means that we won't
  -- properly react to changes to these values, which could be a pretty nasty
  -- gotcha for someone down the line. :(
  borderInfo <- getBorderInfo image

  let setPixbuf force allocation = do
        _width <- Gdk.getRectangleWidth allocation
        _height <- Gdk.getRectangleHeight allocation

        let width = max 1 $ _width - fromIntegral (borderWidth borderInfo)
            height = max 1 $ _height - fromIntegral (borderHeight borderInfo)
            size =
              case orientation of
                Gtk.OrientationHorizontal -> height
                _ -> width

        previousSize <- MV.readMVar lastAllocation

        when (size /= previousSize || force) $ do
          MV.modifyMVar_ lastAllocation $ const $ return size

          pixbuf <- getPixbuf size
          pbWidth <- fromMaybe 0 <$> traverse Gdk.getPixbufWidth pixbuf
          pbHeight <- fromMaybe 0 <$> traverse Gdk.getPixbufHeight pixbuf
          let pbSize = case orientation of
                Gtk.OrientationHorizontal -> pbHeight
                _ -> pbWidth
              logLevel = if pbSize <= size then DEBUG else WARNING

          imageLog logLevel $
            printf
              "Allocating image: size %s, width %s, \
              \ height %s, aw: %s, ah: %s, pbw: %s pbh: %s"
              (show size)
              (show width)
              (show height)
              (show _width)
              (show _height)
              (show pbWidth)
              (show pbHeight)

          Gtk.imageSetFromPixbuf image pixbuf
          postGUIASync $ Gtk.widgetQueueResize image

  _ <- Gtk.onWidgetSizeAllocate image $ setPixbuf False
  return $ Gtk.widgetGetAllocation image >>= setPixbuf True

-- | Make a new "Gtk.Image" and call "autoSizeImage" on it. Automatically scale
-- the "Gdk.Pixbuf" returned from the provided getter to the appropriate size
-- using "scalePixbufToSize".
autoSizeImageNew ::
  (MonadIO m) =>
  (Int32 -> IO Gdk.Pixbuf) -> Gtk.Orientation -> m Gtk.Image
autoSizeImageNew getPixBuf orientation = do
  image <- Gtk.imageNew
  void $
    autoSizeImage
      image
      (\size -> Just <$> (getPixBuf size >>= scalePixbufToSize size orientation))
      orientation
  return image

-- | Make a new "Gtk.MenuItem" that has both a label and an icon.
imageMenuItemNew ::
  (MonadIO m) =>
  T.Text -> (Int32 -> IO (Maybe Gdk.Pixbuf)) -> m Gtk.MenuItem
imageMenuItemNew labelText pixbufGetter = do
  box <- Gtk.boxNew Gtk.OrientationHorizontal 0
  label <- Gtk.labelNew $ Just labelText
  image <- Gtk.imageNew
  void $ autoSizeImage image pixbufGetter Gtk.OrientationHorizontal
  item <- Gtk.menuItemNew
  Gtk.containerAdd box image
  Gtk.containerAdd box label
  Gtk.containerAdd item box
  Gtk.widgetSetHalign box Gtk.AlignStart
  Gtk.widgetSetHalign image Gtk.AlignStart
  Gtk.widgetSetValign box Gtk.AlignFill
  return item
