module System.Taffybar.Widget.Generic.AutoSizeImage where

import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Int
import qualified GI.Gdk as Gdk
import           GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import           StatusNotifier.Tray (scalePixbufToSize)
import           System.Log.Logger
import           System.Taffybar.Util
import           Text.Printf

imageLog :: Priority -> String -> IO ()
imageLog = logM "System.Taffybar.Widget.Generic.AutoSizeImage"

-- | Make a new image and call "autoSizeImage" on it. Automatically scale the
-- pixbuf returned from the provided getter to the appropriate size. Ignores the
-- refresh argument provided by "autoSizeImage".
autoSizeImageNew
  :: MonadIO m
  => (Int32 -> IO Gdk.Pixbuf) -> Gtk.Orientation -> m Gtk.Image
autoSizeImageNew getPixBuf orientation = do
  image <- Gtk.imageNew
  void $ autoSizeImage image
         (\size -> Just <$> (getPixBuf size >>= scalePixbufToSize size orientation))
         orientation
  return image

-- | Automatically update an images pixbuf using the provided action whenever
-- the image is allocated. Returns an action that forces a refresh of the image
-- through the provided action.
autoSizeImage
  :: MonadIO m
  => Gtk.Image
  -> (Int32 -> IO (Maybe Gdk.Pixbuf))
  -> Gtk.Orientation
  -> m (IO ())
autoSizeImage image getPixBuf orientation = liftIO $ do
  Gtk.widgetSetVexpand image True
  lastAllocation <- MV.newMVar 0
  let setPixbuf force allocation = do
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
        when (requestResize || force) $ do
          imageLog DEBUG "Requesting resize"
          getPixBuf size >>= Gtk.imageSetFromPixbuf image
          runOnUIThread $ Gtk.widgetQueueResize image

  _ <- Gtk.onWidgetSizeAllocate image $ setPixbuf False
  return $ Gtk.widgetGetAllocation image >>= setPixbuf True
