module StatusNotifier.Icon.Pixbuf
  ( Rgb8 (..),
    rgb8FromGdkRGBA,
    recolorPixbufMonochrome,
    recolorPixbufMonochromeRGBA,
    recolorPixbufDuotone,
    recolorPixbufDuotoneRGBA,
  )
where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf

data Rgb8 = Rgb8
  { rgb8Red :: !Word8,
    rgb8Green :: !Word8,
    rgb8Blue :: !Word8
  }
  deriving (Eq, Show)

clamp01 :: Double -> Double
clamp01 x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x

toWord8 :: Double -> Word8
toWord8 x = round (clamp01 x * 255.0)

rgb8FromGdkRGBA :: Gdk.RGBA -> IO Rgb8
rgb8FromGdkRGBA rgba = do
  r <- Gdk.getRGBARed rgba
  g <- Gdk.getRGBAGreen rgba
  b <- Gdk.getRGBABlue rgba
  pure $ Rgb8 (toWord8 r) (toWord8 g) (toWord8 b)

lerpWord8 :: Double -> Word8 -> Word8 -> Word8
lerpWord8 t a b =
  let ta = fromIntegral a :: Double
      tb = fromIntegral b :: Double
      out = ta + clamp01 t * (tb - ta)
   in fromIntegral (max (0 :: Int) (min 255 (round out)))

pixelLuminance01 :: Word8 -> Word8 -> Word8 -> Double
pixelLuminance01 r g b =
  let rf = fromIntegral r / 255.0 :: Double
      gf = fromIntegral g / 255.0 :: Double
      bf = fromIntegral b / 255.0 :: Double
   in clamp01 (0.2126 * rf + 0.7152 * gf + 0.0722 * bf)

-- | Replace the RGB channels of the pixbuf with the given color while preserving
-- the alpha channel. This is the "extract shapes and recolor" primitive: the
-- icon's shape comes from its alpha mask.
recolorPixbufMonochrome :: Rgb8 -> GdkPixbuf.Pixbuf -> IO (Maybe GdkPixbuf.Pixbuf)
recolorPixbufMonochrome (Rgb8 r g b) pixbuf = do
  mpb <- GdkPixbuf.pixbufCopy pixbuf
  case mpb of
    Nothing -> pure Nothing
    Just pb -> do
      recolorInPlace pb $ \_origR _origG _origB _a -> (r, g, b)
      pure (Just pb)

recolorPixbufMonochromeRGBA :: Gdk.RGBA -> GdkPixbuf.Pixbuf -> IO (Maybe GdkPixbuf.Pixbuf)
recolorPixbufMonochromeRGBA rgba pixbuf = do
  rgb <- rgb8FromGdkRGBA rgba
  recolorPixbufMonochrome rgb pixbuf

-- | Map the original pixel luminance to a color between the two provided colors,
-- while preserving the alpha channel.
recolorPixbufDuotone :: Rgb8 -> Rgb8 -> GdkPixbuf.Pixbuf -> IO (Maybe GdkPixbuf.Pixbuf)
recolorPixbufDuotone (Rgb8 r0 g0 b0) (Rgb8 r1 g1 b1) pixbuf = do
  mpb <- GdkPixbuf.pixbufCopy pixbuf
  case mpb of
    Nothing -> pure Nothing
    Just pb -> do
      recolorInPlace pb $ \origR origG origB _a ->
        let t = pixelLuminance01 origR origG origB
         in ( lerpWord8 t r0 r1,
              lerpWord8 t g0 g1,
              lerpWord8 t b0 b1
            )
      pure (Just pb)

recolorPixbufDuotoneRGBA :: Gdk.RGBA -> Gdk.RGBA -> GdkPixbuf.Pixbuf -> IO (Maybe GdkPixbuf.Pixbuf)
recolorPixbufDuotoneRGBA rgba0 rgba1 pixbuf = do
  c0 <- rgb8FromGdkRGBA rgba0
  c1 <- rgb8FromGdkRGBA rgba1
  recolorPixbufDuotone c0 c1 pixbuf

recolorInPlace ::
  GdkPixbuf.Pixbuf ->
  (Word8 -> Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8)) ->
  IO ()
recolorInPlace pb computeRgb = do
  width <- fromIntegral <$> GdkPixbuf.pixbufGetWidth pb
  height <- fromIntegral <$> GdkPixbuf.pixbufGetHeight pb
  rowStride <- fromIntegral <$> GdkPixbuf.pixbufGetRowstride pb
  nChannels <- fromIntegral <$> GdkPixbuf.pixbufGetNChannels pb
  hasAlpha <- GdkPixbuf.pixbufGetHasAlpha pb
  pixelsBs <- GdkPixbuf.pixbufGetPixels pb

  let bytesPerPixel = nChannels
      alphaOff = if hasAlpha then 3 else (-1)

      -- Hard stop: unexpected pixbuf format.
      validFormat = bytesPerPixel == 3 || bytesPerPixel == 4

      readChannel :: Ptr Word8 -> Int -> IO Word8
      readChannel p off = peekByteOff p off

      writeChannel :: Ptr Word8 -> Int -> Word8 -> IO ()
      writeChannel p off v = pokeByteOff p off v

  if width <= 0 || height <= 0 || not validFormat || BS.null pixelsBs
    then pure ()
    else
      -- 'pixbufGetPixels' gives us a view of the pixbuf's pixel buffer. We mutate
      -- in-place after copying the pixbuf so the caller gets an isolated buffer.
      BSU.unsafeUseAsCString pixelsBs $ \pixels0 -> do
        let pixels :: Ptr Word8
            pixels = castPtr pixels0

            rows :: Int -> Ptr Word8
            rows y = pixels `plusPtr` (y * rowStride)

            pixelPtr :: Ptr Word8 -> Int -> Ptr Word8
            pixelPtr row x = row `plusPtr` (x * bytesPerPixel)

        forM_ [0 .. height - 1] $ \y -> do
          let row = rows y
          forM_ [0 .. width - 1] $ \x -> do
            let p = pixelPtr row x
            a <- if hasAlpha then readChannel p alphaOff else pure 255
            if a == 0
              then pure ()
              else do
                origR <- readChannel p 0
                origG <- readChannel p 1
                origB <- readChannel p 2
                let (outR, outG, outB) = computeRgb origR origG origB a
                writeChannel p 0 outR
                writeChannel p 1 outG
                writeChannel p 2 outB
