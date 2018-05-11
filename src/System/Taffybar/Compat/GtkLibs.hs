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

fromGIWidget :: MonadIO m => GI.Gtk.Widget -> m Gtk.Widget
fromGIWidget (GI.Gtk.Widget wManagedPtr) = liftIO $
  wrapNewGObject Gtk.mkWidget (castPtr <$> disownManagedPtr wManagedPtr)

toGIWidget :: MonadIO m => Gtk.Widget -> m GI.Gtk.Widget
toGIWidget widget = liftIO $ do
  fPtr <- withForeignPtr (Gtk.unWidget widget) $
          flip GI.Gtk.newManagedPtr (return ()) . castPtr
  return $! GI.Gtk.Widget fPtr

toGIWindow :: MonadIO m => Gtk.Window -> m GI.Gtk.Window
toGIWindow window = liftIO $ do
  let wid = Gtk.toWidget window
  fPtr <- withForeignPtr (Gtk.unWidget wid) $ flip GI.Gtk.newManagedPtr (return ()) . castPtr
  return $! GI.Gtk.Window fPtr

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
