module System.Taffybar.Information.Volume
  ( getVolume
  , setVolume
  ) where

import Sound.ALSA.Mixer

volumeAct :: String -> String -> (Volume -> Integer -> Integer -> IO a) -> IO a
volumeAct mix channel action = withMixer mix $ \mixer -> do
  Just control <- getControlByName mixer channel
  let Just playbackVolume = playback $ volume control
  (lo, hi) <- getRange playbackVolume
  action playbackVolume lo hi

-- | Gets volume of a channel of a mixer
getVolume :: String -> String -> IO Integer
getVolume mix channel = volumeAct mix channel $ \playbackVolume lo hi -> do
  Just vol <- getChannel FrontLeft $ value playbackVolume
  return $ toPercent vol lo hi

-- | Sets volume of a channel of a mixer
setVolume :: String -> String -> Double -> IO ()
setVolume mix channel vol = volumeAct mix channel $ \playbackVolume lo hi -> do
  let volumeValue = value playbackVolume
      percentage = fromPercent vol lo hi
      setForChannel c = setChannel c volumeValue percentage
  mapM_ setForChannel allChannels

toPercent :: Integer -> Integer -> Integer -> Integer
toPercent v lo hi = ceiling $ (v' - lo') / (hi' - lo') * 100
  where
    v' :: Double
    v' = fromIntegral v
    lo' = fromIntegral lo
    hi' = fromIntegral hi

fromPercent :: Double -> Integer -> Integer -> Integer
fromPercent v lo hi = ceiling $ lo' + (hi' - lo') * v / 100
  where lo' = fromIntegral lo
        hi' = fromIntegral hi
