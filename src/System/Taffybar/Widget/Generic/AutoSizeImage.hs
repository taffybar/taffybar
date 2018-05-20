{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.Widget.Generic.AutoSizeImage where

import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.GI.Gtk.Threading as Gtk
import           Data.Int
import qualified GI.Gdk as Gdk
import           GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import           StatusNotifier.Tray (scalePixbufToSize)
import           System.Log.Logger
import           System.Taffybar.Widget.Util
import           Text.Printf

imageLog :: Priority -> String -> IO ()
imageLog = logM "System.Taffybar.Widget.Generic.AutoSizeImage"

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

-- | Get the total size of the border (the sum of its assigned margin, border
-- and padding values) that will be drawn for a widget as a "BorderInfo" record.
getBorderInfo :: (MonadIO m, Gtk.IsWidget a) => a -> m BorderInfo
getBorderInfo widget = liftIO $ do
  stateFlags <- Gtk.widgetGetStateFlags widget
  styleContext <- Gtk.widgetGetStyleContext widget

  let getBorderInfoFor borderFn =
        borderFn styleContext stateFlags >>= toBorderInfo
      combineBorderInfo lastSum fn =
        addBorderInfo lastSum <$> getBorderInfoFor fn

  foldM combineBorderInfo borderInfoZero borderFunctions

-- | Get the actual allocation for a "Gtk.Widget", accounting for the size of
-- its CSS assined margin, border and padding values.
getContentAllocation
  :: (MonadIO m, Gtk.IsWidget a)
  => a -> BorderInfo -> m Gdk.Rectangle
getContentAllocation widget borderInfo = do
  borderInfo <- getBorderInfo widget

  allocation <- Gtk.widgetGetAllocation widget
  currentWidth <- Gdk.getRectangleWidth allocation
  currentHeight <- Gdk.getRectangleHeight allocation
  currentX <- Gdk.getRectangleX allocation
  currentY <- Gdk.getRectangleX allocation

  Gdk.setRectangleWidth allocation $ max 1 $
     currentWidth - fromIntegral (borderWidth borderInfo)
  Gdk.setRectangleHeight allocation $ max 1 $
     currentHeight - fromIntegral (borderHeight borderInfo)
  Gdk.setRectangleX allocation $
     currentX + fromIntegral (borderLeft borderInfo)
  Gdk.setRectangleY allocation $
     currentY + fromIntegral (borderTop borderInfo)

  return allocation

-- | Automatically update the "Gdk.Pixbuf" of a "Gtk.Image" using the provided
-- action whenever the "Gtk.Image" is allocated. Returns an action that forces a
-- refresh of the image through the provided action.
autoSizeImage
  :: MonadIO m
  => Gtk.Image
  -> (Int32 -> IO (Maybe Gdk.Pixbuf))
  -> Gtk.Orientation
  -> m (IO ())
autoSizeImage image getPixbuf orientation = liftIO $ do
  case orientation of
    Gtk.OrientationHorizontal -> Gtk.widgetSetVexpand image True
    _ -> Gtk.widgetSetHexpand image True
  widgetSetClassGI image "AutoSizeImage"

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
          imageLog DEBUG $
                 printf "Allocating image: size %s, width %s, \
                         \ height %s, aw: %s, ah: %s"
                 (show size)
                 (show width)
                 (show height)
                 (show _width)
                 (show _height)
          getPixbuf size >>= Gtk.imageSetFromPixbuf image
          Gtk.postGUIASync $ Gtk.widgetQueueResize image

  _ <- Gtk.onWidgetSizeAllocate image $ setPixbuf False
  return $ Gtk.widgetGetAllocation image >>= setPixbuf True

-- | Make a new "Gtk.Image" and call "autoSizeImage" on it. Automatically scale
-- the "Gdk.Pixbuf" returned from the provided getter to the appropriate size
-- using "scalePixbufToSize".
autoSizeImageNew
  :: MonadIO m
  => (Int32 -> IO Gdk.Pixbuf) -> Gtk.Orientation -> m Gtk.Image
autoSizeImageNew getPixBuf orientation = do
  image <- Gtk.imageNew
  void $ autoSizeImage image
         (\size -> Just <$> (getPixBuf size >>= scalePixbufToSize size orientation))
         orientation
  return image
