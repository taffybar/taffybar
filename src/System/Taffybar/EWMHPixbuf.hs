-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.EWMHPixbuf
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------

module System.Taffybar.EWMHPixbuf
  ( ColorRGBA
  , pixBufFromEWMHIcon
  , pixelsARGBToBytesABGR
  , pixBufFromColor
  , pixBufFromFile
  ) where

import           Data.Bits
import           Data.Int
import           Data.Word
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import qualified GI.GdkPixbuf.Enums as Gdk
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import           System.Taffybar.Compat.GtkLibs
import           System.Taffybar.Information.EWMHDesktopInfo

type ColorRGBA = Word32

colorspace :: Gdk.Colorspace
colorspace = Gdk.ColorspaceRgb

-- | Create a pixbuf from the pixel data in an EWMHIcon.
pixBufFromEWMHIcon :: EWMHIcon -> IO Gdk.Pixbuf
pixBufFromEWMHIcon EWMHIcon {width = w, height = h, pixelsARGB = px} = do
  wPtr <- pixelsARGBToBytesABGR px (w * h)
  pixbufNewFromData wPtr w h

-- | Create a pixbuf with the indicated RGBA color.
pixBufFromColor :: Int32 -> ColorRGBA -> IO Gdk.Pixbuf
pixBufFromColor imgSize c = do
  Just pixbuf <- Gdk.pixbufNew colorspace True 8 imgSize imgSize
  Gdk.pixbufFill pixbuf c
  return pixbuf

-- | Convert a C array of integer pixels in the ARGB format to the ABGR format.
-- Returns an unmanged Ptr that points to a block of memory that must be freed
-- manually.
pixelsARGBToBytesABGR
  :: (Storable a, Bits a, Num a, Integral a)
  => Ptr a -> Int -> IO (Ptr Word8)
pixelsARGBToBytesABGR ptr size = do
  target <- mallocArray (size * 4)
  let writeIndex i = do
        bits <- peekElemOff ptr i
        let b = toByte bits
            g = toByte $ bits `shift` (-8)
            r = toByte $ bits `shift` (-16)
            a = toByte $ bits `shift` (-24)
            baseTarget = 4 * i
            doPoke offset = pokeElemOff target (baseTarget + offset)
            toByte = fromIntegral . (.&. 0xFF)
        doPoke 0 r
        doPoke 1 g
        doPoke 2 b
        doPoke 3 a
      writeIndexAndNext i
        | i >= size = return ()
        | otherwise = writeIndex i >> writeIndexAndNext (i + 1)
  writeIndexAndNext 0
  return target

-- | Create a pixbuf from a file and scale it to be square.
pixBufFromFile :: FilePath -> IO Gdk.Pixbuf
pixBufFromFile = Gdk.pixbufNewFromFile
