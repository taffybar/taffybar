module System.Taffybar.WindowIcon where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Bits
import qualified Data.Char as Char
import           Data.Either.Combinators
import           Data.Int
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Ord
import           Data.Word
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import qualified GI.GdkPixbuf.Enums as Gdk
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import           System.Log.Logger
import           System.Taffybar.Compat.GtkLibs
import           System.Taffybar.Information.EWMHDesktopInfo
import           System.Taffybar.Information.X11DesktopInfo
import           System.Taffybar.Information.XDG.DesktopEntry
import           System.Taffybar.Util
import           System.Taffybar.Widget.Util
import           Text.Printf

type ColorRGBA = Word32

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

selectEWMHIcon :: Int32 -> [EWMHIcon] -> Maybe EWMHIcon
selectEWMHIcon imgSize icons = listToMaybe prefIcon
  where sortedIcons = sortBy (comparing height) icons
        smallestLargerIcon = take 1 $ dropWhile ((<= fromIntegral imgSize) . height) sortedIcons
        largestIcon = take 1 $ reverse sortedIcons
        prefIcon = smallestLargerIcon ++ largestIcon

getPixbufFromEWMHIcons :: Int32 -> [EWMHIcon] -> IO (Maybe Gdk.Pixbuf)
getPixbufFromEWMHIcons size = traverse pixBufFromEWMHIcon . selectEWMHIcon size

-- | Create a pixbuf from the pixel data in an EWMHIcon.
pixBufFromEWMHIcon :: EWMHIcon -> IO Gdk.Pixbuf
pixBufFromEWMHIcon EWMHIcon {width = w, height = h, pixelsARGB = px} = do
  let width = fromIntegral w
      height = fromIntegral h
      rowStride = width * 4
  wPtr <- pixelsARGBToBytesABGR px (w * h)
  Gdk.pixbufNewFromData wPtr Gdk.ColorspaceRgb True 8
     width height rowStride (Just free)

getIconPixBufFromEWMH :: Int32 -> X11Window -> X11Property (Maybe Gdk.Pixbuf)
getIconPixBufFromEWMH size x11WindowId = runMaybeT $ do
  ewmhData <- MaybeT $ getWindowIconsData x11WindowId
  MaybeT $ lift $ withEWMHIcons ewmhData (getPixbufFromEWMHIcons size)

-- | Create a pixbuf with the indicated RGBA color.
pixBufFromColor imgSize c = do
  Just pixbuf <- Gdk.pixbufNew Gdk.ColorspaceRgb True 8 imgSize imgSize
  Gdk.pixbufFill pixbuf c
  return pixbuf



getWindowIconFromExactClass size klass = runMaybeT $ do
  logPrintF "System.Taffybar.WindowIcon" DEBUG "Using class: %s" klass
  entry <- MaybeT $ getDirectoryEntryDefault klass
  logPrintF "System.Taffybar.WindowIcon" DEBUG "Got entry: %s" entry
  MaybeT $ getImageForDesktopEntry size entry

getWindowIconFromClass size klass =
  foldl combine (return Nothing) $ splitOn "\NUL" klass
  where
    combine soFar theClass = maybeTCombine soFar (getWindowIconFromExactClass size theClass)
