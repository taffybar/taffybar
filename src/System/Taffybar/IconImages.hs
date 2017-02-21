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
  pixelsARGBToBytesRGBA,
  pixBufFromColor,
  pixBufFromFile,
  selectEWMHIcon
) where

import           Data.List
import           Data.Ord
import           Data.Word (Word8)
import           Foreign.C.Types (CUChar(..))
import           Foreign.Marshal.Array (newArray)
import qualified Graphics.UI.Gtk as Gtk
import           System.Information.EWMHDesktopInfo

type ColorRGBA = (Word8, Word8, Word8, Word8)

-- | Take the passed in pixbuf and ensure its scaled square.
scalePixbuf :: Int -> Gtk.Pixbuf -> IO Gtk.Pixbuf
scalePixbuf imgSize pixbuf = do
  h <- Gtk.pixbufGetHeight pixbuf
  w <- Gtk.pixbufGetWidth pixbuf
  if h /= imgSize || w /= imgSize
  then
    Gtk.pixbufScaleSimple pixbuf imgSize imgSize Gtk.InterpBilinear
  else
    return pixbuf

-- | Create a pixbuf from the pixel data in an EWMHIcon,
-- scale it square, and set it in a GTK Image.
pixBufFromEWMHIcon :: EWMHIcon -> IO Gtk.Pixbuf
pixBufFromEWMHIcon EWMHIcon {width = w, height = h, pixelsARGB = px} = do
  let pixelsPerRow = w
      bytesPerPixel = 4
      rowStride = pixelsPerRow * bytesPerPixel
      sampleBits = 8
      hasAlpha = True
      colorspace = Gtk.ColorspaceRgb
      bytesRGBA = pixelsARGBToBytesRGBA px
  cPtr <- newArray $ map CUChar bytesRGBA
  Gtk.pixbufNewFromData cPtr colorspace hasAlpha sampleBits w h rowStride

-- | Create a pixbuf with the indicated RGBA color,
-- scale it square, and set it in a GTK Image.
pixBufFromColor :: Int -> ColorRGBA -> IO Gtk.Pixbuf
pixBufFromColor imgSize (r, g, b, a) = do
  let sampleBits = 8
      hasAlpha = True
      colorspace = Gtk.ColorspaceRgb
  pixbuf <- Gtk.pixbufNew colorspace hasAlpha sampleBits imgSize imgSize
  Gtk.pixbufFill pixbuf r g b a
  return pixbuf

-- | Convert a list of integer pixels to a bytestream with 4 channels.
pixelsARGBToBytesRGBA :: [Int] -> [Word8]
pixelsARGBToBytesRGBA (x:xs) = r:g:b:a:pixelsARGBToBytesRGBA xs
  where r = toByte $ x `div` 0x10000   `mod` 0x100
        g = toByte $ x `div` 0x100     `mod` 0x100
        b = toByte $ x                 `mod` 0x100
        a = toByte $ x `div` 0x1000000 `mod` 0x100
        toByte i = (fromIntegral i) :: Word8
pixelsARGBToBytesRGBA _ = []

-- | Create a pixbuf from a file,
-- scale it square, and set it in a GTK Image.
pixBufFromFile :: Int -> FilePath -> IO Gtk.Pixbuf
pixBufFromFile imgSize file = Gtk.pixbufNewFromFileAtScale file imgSize imgSize False

selectEWMHIcon :: Int -> [EWMHIcon] -> EWMHIcon
selectEWMHIcon imgSize icons = head prefIcon
  where sortedIcons = sortBy (comparing height) icons
        smallestLargerIcon = take 1 $ dropWhile ((<= imgSize) . height) sortedIcons
        largestIcon = take 1 $ reverse sortedIcons
        prefIcon = smallestLargerIcon ++ largestIcon
