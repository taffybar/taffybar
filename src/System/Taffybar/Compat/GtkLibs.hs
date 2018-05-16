-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Compat.GtkLibs
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------
module System.Taffybar.Compat.GtkLibs where

import           Control.Monad.IO.Class
import           Data.GI.Base.ManagedPtr
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           GI.GdkPixbuf.Enums
import qualified GI.GdkPixbuf.Objects.Pixbuf as PB
import qualified GI.Gtk
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Types as Gtk
import           System.Glib.GObject

fromGIPixBuf :: MonadIO m => PB.Pixbuf -> m Gtk.Pixbuf
fromGIPixBuf (PB.Pixbuf pbManagedPtr) = liftIO $
  wrapNewGObject Gtk.mkPixbuf (castPtr <$> disownManagedPtr pbManagedPtr)

fromGIWidget'
  :: (MonadIO m, GObjectClass a)
  => (ForeignPtr a -> a, FinalizerPtr a)
  -> GI.Gtk.ManagedPtr b
  -> m a
fromGIWidget' mk2hs giWidget = liftIO $
  wrapNewGObject mk2hs (castPtr <$> disownManagedPtr giWidget)

toGIWidget'
  :: MonadIO m
  => (GI.Gtk.ManagedPtr a1 -> b) -> (t -> ForeignPtr a2) -> t -> m b
toGIWidget' mkGI un2hs gtk2hsWidget = liftIO $ do
  fPtr <- withForeignPtr (un2hs gtk2hsWidget) $
          flip GI.Gtk.newManagedPtr (return ()) . castPtr
  return $! mkGI fPtr

fromGIWidget :: MonadIO m => GI.Gtk.Widget -> m Gtk.Widget
fromGIWidget (GI.Gtk.Widget wManagedPtr) = fromGIWidget' Gtk.mkWidget wManagedPtr

toGIWidget :: MonadIO m => Gtk.Widget -> m GI.Gtk.Widget
toGIWidget = toGIWidget' GI.Gtk.Widget Gtk.unWidget

toGIWindow :: MonadIO m => Gtk.Window -> m GI.Gtk.Window
toGIWindow = toGIWidget' GI.Gtk.Window Gtk.unWindow

fromGIImage :: MonadIO m => GI.Gtk.Image -> m Gtk.Image
fromGIImage (GI.Gtk.Image wManagedPtr) = fromGIWidget' Gtk.mkImage wManagedPtr

toGIImage :: MonadIO m => Gtk.Image -> m GI.Gtk.Image
toGIImage = toGIWidget' GI.Gtk.Image Gtk.unImage

-- | Call the GI version of 'pixbufNewFromData' with sensible parameters. The
-- provided ptr will be freed when the pixbuf is destroyed.
pixbufNewFromData :: (Integral p2, Integral p1) => Ptr Word8 -> p2 -> p1 -> IO Gtk.Pixbuf
pixbufNewFromData ptr w h = do
  let width = fromIntegral w
      height = fromIntegral h
      rowStride = width * 4
  giPb <- PB.pixbufNewFromData ptr ColorspaceRgb True 8
    width height rowStride (Just free)
  fromGIPixBuf giPb
