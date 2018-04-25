module System.Taffybar.Information.Volume (
  getVolume,
  setVolume
) where

import Sound.ALSA.Mixer

-- | Gets volume of a channel of a mixer
getVolume :: String -> String -> IO Integer
getVolume mix channel = withMixer mix $ \mixer -> do
  Just control <- getControlByName mixer channel
  let Just playbackVolume = playback $ volume control
  (lo, hi) <- getRange playbackVolume
  Just vol <- getChannel FrontLeft $ value playbackVolume
  return $ toPercent vol lo hi

-- | Sets volume of a channel of a mixer
setVolume :: String -> String -> Double -> IO ()
setVolume mix channel vol = withMixer mix $ \mixer -> do
  Just control <- getControlByName mixer channel
  let Just playbackVolume = playback $ volume control
  (lo, hi) <- getRange playbackVolume
  setChannel FrontLeft (value playbackVolume) $ fromPercent vol lo hi
  setChannel FrontRight (value playbackVolume) $ fromPercent vol lo hi
  setChannel RearLeft (value playbackVolume) $ fromPercent vol lo hi
  setChannel RearRight (value playbackVolume) $ fromPercent vol lo hi
  setChannel FrontCenter (value playbackVolume) $ fromPercent vol lo hi

toPercent :: Integer -> Integer -> Integer -> Integer
toPercent v lo hi = ceiling $ (v' - lo') / (hi' - lo') * 100
  where v' = fromIntegral v
        lo' = fromIntegral lo
        hi' = fromIntegral hi

fromPercent :: Double -> Integer -> Integer -> Integer
fromPercent v lo hi = ceiling $ lo' + (hi' - lo') * v / 100
  where lo' = fromIntegral lo
        hi' = fromIntegral hi
