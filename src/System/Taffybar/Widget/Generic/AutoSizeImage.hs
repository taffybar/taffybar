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

borderFunctions :: [Gtk.StyleContext -> [Gtk.StateFlags] -> IO Gtk.Border]
borderFunctions =
  [ Gtk.styleContextGetPadding
  , Gtk.styleContextGetMargin
  , Gtk.styleContextGetBorder
  ]

data BorderInfo = BorderInfo
  { borderTop :: Int16
  , borderBottom :: Int16
  , borderLeft :: Int16
  , borderRight :: Int16
  } deriving (Show, Eq)

borderInfoZero :: BorderInfo
borderInfoZero = BorderInfo 0 0 0 0

borderWidth, borderHeight :: BorderInfo -> Int16
borderWidth borderInfo = borderLeft borderInfo + borderRight borderInfo
borderHeight borderInfo = borderTop borderInfo + borderBottom borderInfo


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
  (BorderInfo t2 b2 l2 r2)
    = BorderInfo (t1 + t2) (b1 + b2) (l1 + l2) (r1 + r2)

-- TODO: Move this to util when gtk migration is done
getContentAllocation :: (MonadIO m, Gtk.IsWidget a) => a -> m Gdk.Rectangle
getContentAllocation widget = liftIO $ do
  allocation <- Gtk.widgetGetAllocation widget
  currentWidth <- Gdk.getRectangleWidth allocation
  currentHeight <- Gdk.getRectangleHeight allocation
  currentX <- Gdk.getRectangleX allocation
  currentY <- Gdk.getRectangleX allocation

  stateFlags <- Gtk.widgetGetStateFlags widget
  styleContext <- Gtk.widgetGetStyleContext widget

  let getBorderInfoFor borderFn =
        borderFn styleContext stateFlags >>= toBorderInfo
      combineBorderInfo lastSum fn =
        addBorderInfo lastSum <$> getBorderInfoFor fn

  borderInfo <- foldM combineBorderInfo borderInfoZero borderFunctions

  Gdk.setRectangleWidth allocation $
     currentWidth - fromIntegral (borderWidth borderInfo)
  Gdk.setRectangleHeight allocation $
     currentHeight - fromIntegral (borderHeight borderInfo)
  Gdk.setRectangleX allocation $
     currentX + fromIntegral (borderLeft borderInfo)
  Gdk.setRectangleY allocation $
     currentY + fromIntegral (borderTop borderInfo)
  return allocation

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
  let setPixbuf force actualAllocation = do
        allocation <- getContentAllocation image
        currentWidth <- Gdk.getRectangleWidth allocation
        currentHeight <- Gdk.getRectangleHeight allocation
        actualWidth <- Gdk.getRectangleWidth actualAllocation
        actualHeight <- Gdk.getRectangleHeight actualAllocation

        let size =
              case orientation of
                Gtk.OrientationHorizontal -> currentHeight
                _ -> currentWidth

        requestResize <- MV.modifyMVar lastAllocation $ \previous ->
          return (size, size /= previous)
        imageLog DEBUG $
                 printf "Allocating image: size %s, width %s, \
                         \ height %s, do resize %s, aw: %s, ah: %s"
                 (show size)
                 (show currentWidth)
                 (show currentHeight)
                 (show requestResize)
                 (show actualWidth)
                 (show actualHeight)
        when (requestResize || force) $ do
          imageLog DEBUG "Requesting resize"
          getPixBuf size >>= Gtk.imageSetFromPixbuf image
          runOnUIThread $ Gtk.widgetQueueResize image

  _ <- Gtk.onWidgetSizeAllocate image $ setPixbuf False
  return $ Gtk.widgetGetAllocation image >>= setPixbuf True
