module System.Taffybar.Widget.Generic.AutoSizeImage where

import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.Trans
import           Data.Int
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf.Enums as Gdk
import           GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import           System.Log.Logger
import           System.Taffybar.Util
import           Text.Printf

imageLog :: Priority -> String -> IO ()
imageLog = logM "System.Taffybar.Widget.Generic.AutoSizeImage"

scalePixbufToSize :: Int32 -> Gtk.Orientation -> Gdk.Pixbuf -> IO Gdk.Pixbuf
scalePixbufToSize size orientation pixbuf = do
  width <- Gdk.pixbufGetWidth pixbuf
  height <- Gdk.pixbufGetHeight pixbuf
  imageLog DEBUG $ printf "Scaling pixbuf to %s, actualW: %s, actualH: %s"
           (show size) (show width) (show height)
  let getRatio :: Int32 -> Rational
      getRatio toScale =
        fromIntegral size / fromIntegral toScale
      getOther :: Int32 -> Int32 -> Int32
      getOther toScale other = floor $ getRatio toScale * fromIntegral other
      warnAndReturnOrig =
        imageLog WARNING "Unable to scale pixbuf" >> return pixbuf
      doScale w h = do
        imageLog DEBUG $ printf "targetW: %s, targetH: %s"
           (show width) (show height)
        Gdk.pixbufScaleSimple pixbuf w h Gdk.InterpTypeBilinear
  maybe warnAndReturnOrig return =<< case orientation of
    Gtk.OrientationHorizontal -> doScale (getOther height width) size
    _ -> doScale size $ getOther width height

autoSizeImageNew
  :: MonadIO m
  => (Int32 -> IO Gdk.Pixbuf) -> Gtk.Orientation -> m Gtk.Image
autoSizeImageNew getPixBuf orientation = liftIO $ do
  image <- Gtk.imageNew
  Gtk.widgetSetVexpand image True
  lastAllocation <- MV.newMVar 0
  let setPixbuf allocation = do
        currentWidth <- Gdk.getRectangleWidth allocation
        currentHeight <- Gdk.getRectangleHeight allocation

        let size =
              case orientation of
                Gtk.OrientationHorizontal -> currentHeight
                _ -> currentWidth

        requestResize <- MV.modifyMVar lastAllocation $ \previous ->
          return (size, size /= previous)
        imageLog DEBUG $
                 printf "Allocating image: size %s, width %s, \
                         \ height %s, do resize %s"
                 (show size)
                 (show currentWidth)
                 (show currentHeight)
                 (show requestResize)
        when requestResize $ do
          imageLog DEBUG "Requesting resize"
          -- Gtk.widgetSetSizeRequest image size size
          getPixBuf size >>= scalePixbufToSize size orientation >>= Gtk.imageSetFromPixbuf image . Just
          runOnUIThread $ Gtk.widgetQueueResize image

  _ <- Gtk.onWidgetSizeAllocate image setPixbuf
  return image
