module System.Information.Volume (
  -- * Accessors
  getVolume
) where

import Sound.ALSA.Mixer

-- | Gets volume of a control of a mixer
getVolume :: String -> String -> IO Integer
getVolume mixer control = do
  Just control <- getControlByName mixer control
  let Just playbackVolume = playback $ volume control
  (min, max) <- getRange playbackVolume
  Just vol <- getChannel FrontLeft $ value $ playbackVolume
  return . ceiling $ percent vol min max

  where
  percent :: Integer -> Integer -> Integer -> Float
  percent v' lo' hi' = (v - lo) / (hi - lo) * 100
    where v = fromIntegral v'
          lo = fromIntegral lo'
          hi = fromIntegral hi'
