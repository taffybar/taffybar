-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.IconImages
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------

module System.Taffybar.IconImages (
  ColorRGBA,
  scalePixbuf,
  pixBufFromEWMHIcon,
  pixelsARGBToBytesABGR,
  pixBufFromColor,
  pixBufFromFile,
  selectEWMHIcon
) where

-- TODO: rename module to IconPixbuf

import           Data.Bits
import qualified Data.List as L
import           Data.Ord ( comparing )
import           Data.Word
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import qualified Graphics.UI.Gtk as Gtk
import           System.Information.EWMHDesktopInfo
import           System.Taffybar.GtkLibCompat

type ColorRGBA = (Word8, Word8, Word8, Word8)

-- | Take the passed in pixbuf and scale it to the provided imageSize.
scalePixbuf :: Int -> Gtk.Pixbuf -> IO Gtk.Pixbuf
scalePixbuf imgSize pixbuf = do
  h <- Gtk.pixbufGetHeight pixbuf
  w <- Gtk.pixbufGetWidth pixbuf
  if h /= imgSize || w /= imgSize
  then
    Gtk.pixbufScaleSimple pixbuf imgSize imgSize Gtk.InterpBilinear
  else
    return pixbuf

sampleBits :: Int
sampleBits = 8

hasAlpha :: Bool
hasAlpha = True

colorspace :: Gtk.Colorspace
colorspace = Gtk.ColorspaceRgb

-- | Create a pixbuf from the pixel data in an EWMHIcon.
pixBufFromEWMHIcon :: EWMHIcon -> IO Gtk.Pixbuf
pixBufFromEWMHIcon EWMHIcon {width = w, height = h, pixelsARGB = px} = do
  wPtr <- pixelsARGBToBytesABGR px (w * h)
  pixbufNewFromData wPtr w h
  -- Gtk.pixbufNewFromData (castPtr wPtr) colorspace hasAlpha sampleBits w h (w * 4)

-- | Create a pixbuf with the indicated RGBA color.
pixBufFromColor :: Int -> ColorRGBA -> IO Gtk.Pixbuf
pixBufFromColor imgSize (r, g, b, a) = do
  pixbuf <- Gtk.pixbufNew colorspace hasAlpha sampleBits imgSize imgSize
  Gtk.pixbufFill pixbuf r g b a
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
pixBufFromFile :: Int -> FilePath -> IO Gtk.Pixbuf
pixBufFromFile imgSize file =
  Gtk.pixbufNewFromFileAtScale file imgSize imgSize False

selectEWMHIcon :: Int -> [EWMHIcon] -> EWMHIcon
selectEWMHIcon imgSize icons = head prefIcon
  where sortedIcons = L.sortBy (comparing height) icons
        smallestLargerIcon = take 1 $ dropWhile ((<= imgSize) . height) sortedIcons
        largestIcon = take 1 $ reverse sortedIcons
        prefIcon = smallestLargerIcon ++ largestIcon
