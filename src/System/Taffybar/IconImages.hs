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

import           Data.Bits
import qualified Data.List as L
import           Data.Ord ( comparing )
import           Data.Word (Word8, Word32)
import           Foreign.C.Types (CUChar(..))
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
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

sampleBits :: Int
sampleBits = 8

hasAlpha :: Bool
hasAlpha = True

colorspace :: Gtk.Colorspace
colorspace = Gtk.ColorspaceRgb

-- | Create a pixbuf from the pixel data in an EWMHIcon,
-- scale it square, and set it in a GTK Image.
pixBufFromEWMHIcon :: EWMHIcon -> IO Gtk.Pixbuf
pixBufFromEWMHIcon EWMHIcon {width = w, height = h, pixelsARGB = px} = do
  wPtr <- pixelsARGBToBytesRGBAa px (w*h)
  let pixelsPerRow = w
      bytesPerPixel = 4
      rowStride = pixelsPerRow * bytesPerPixel
      cPtr = castPtr wPtr
  Gtk.pixbufNewFromData cPtr colorspace hasAlpha sampleBits w h rowStride

-- | Create a pixbuf with the indicated RGBA color,
-- scale it square, and set it in a GTK Image.
pixBufFromColor :: Int -> ColorRGBA -> IO Gtk.Pixbuf
pixBufFromColor imgSize (r, g, b, a) = do
  pixbuf <- Gtk.pixbufNew colorspace hasAlpha sampleBits imgSize imgSize
  Gtk.pixbufFill pixbuf r g b a
  return pixbuf

pixelsARGBToBytesRGBAa = pixelsARGBToBytesABGR

-- | Convert a list of integer pixels to a bytestream with 4 channels.
pixelsARGBToBytesABGR
  :: (Storable a, Bits a, Num a, Integral a)
  => Ptr a -> Int -> IO (Ptr CUChar)
pixelsARGBToBytesABGR ptr size = do
  target <- callocArray (size * 4)
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

mapArr :: (Storable a, Storable b, Show b) => (a -> b) -> Ptr a -> Int -> IO (Ptr b)
mapArr fn ptr size = do
  target <- callocArray size
  let writeIndex i =
        (fn <$> peekElemOff ptr i) >>=
        pokeElemOff target i
      writeIndexAndNext i
        | i >= size = return ()
        | otherwise = writeIndex i >> writeIndexAndNext (i + 1)
  writeIndexAndNext 0
  return target

-- | Create a pixbuf from a file,
-- scale it square, and set it in a GTK Image.
pixBufFromFile :: Int -> FilePath -> IO Gtk.Pixbuf
pixBufFromFile imgSize file = Gtk.pixbufNewFromFileAtScale file imgSize imgSize False

selectEWMHIcon :: Int -> [EWMHIcon] -> EWMHIcon
selectEWMHIcon imgSize icons = head prefIcon
  where sortedIcons = L.sortBy (comparing height) icons
        smallestLargerIcon = take 1 $ dropWhile ((<= imgSize) . height) sortedIcons
        largestIcon = take 1 $ reverse sortedIcons
        prefIcon = smallestLargerIcon ++ largestIcon
