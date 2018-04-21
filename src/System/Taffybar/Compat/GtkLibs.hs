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

fromGIPixBuf :: PB.Pixbuf -> IO Gtk.Pixbuf
fromGIPixBuf (PB.Pixbuf pbManagedPtr) =
  wrapNewGObject Gtk.mkPixbuf (castPtr <$> disownManagedPtr pbManagedPtr)

toGIWindow :: Gtk.Window -> IO GI.Gtk.Window
toGIWindow window = do
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
