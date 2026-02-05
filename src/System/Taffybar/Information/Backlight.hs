----------------------------------------------------------------------------- 
-- |
-- Module      : System.Taffybar.Information.Backlight
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Simple helpers to read backlight status from /sys/class/backlight.
--
-----------------------------------------------------------------------------

module System.Taffybar.Information.Backlight
  ( BacklightInfo(..)
  , getBacklightDevices
  , getBacklightInfo
  ) where

import Control.Exception (SomeException, catch)
import Data.List (sort)
import Data.Maybe (listToMaybe)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Text.Read (readMaybe)

backlightBasePath :: FilePath
backlightBasePath = "/sys/class/backlight"

-- | Information about a backlight device.
data BacklightInfo = BacklightInfo
  { backlightDevice :: FilePath
  , backlightBrightness :: Int
  , backlightMaxBrightness :: Int
  , backlightPercent :: Int
  } deriving (Eq, Show)

-- | List all backlight device names under /sys/class/backlight.
getBacklightDevices :: IO [FilePath]
getBacklightDevices =
  catch (sort <$> listDirectory backlightBasePath) $ \(_ :: SomeException) ->
    return []

-- | Get backlight info for the provided device name, or the first device found
-- if no name is provided.
getBacklightInfo :: Maybe FilePath -> IO (Maybe BacklightInfo)
getBacklightInfo deviceOverride = do
  device <- selectDevice deviceOverride
  case device of
    Nothing -> return Nothing
    Just dev -> readDevice dev

selectDevice :: Maybe FilePath -> IO (Maybe FilePath)
selectDevice (Just device) = do
  let path = backlightBasePath </> device
  exists <- doesDirectoryExist path
  return $ if exists then Just device else Nothing
selectDevice Nothing = listToMaybe <$> getBacklightDevices

readDevice :: FilePath -> IO (Maybe BacklightInfo)
readDevice device = do
  let basePath = backlightBasePath </> device
  brightness <- readIntFile (basePath </> "brightness")
  maxBrightness <- readIntFile (basePath </> "max_brightness")
  case (brightness, maxBrightness) of
    (Just current, Just maxVal) | maxVal > 0 -> do
      let percent = round $ (fromIntegral current * 100 :: Double) / fromIntegral maxVal
      return $ Just BacklightInfo
        { backlightDevice = device
        , backlightBrightness = current
        , backlightMaxBrightness = maxVal
        , backlightPercent = percent
        }
    _ -> return Nothing

readIntFile :: FilePath -> IO (Maybe Int)
readIntFile path =
  catch (do
    contents <- readFile path
    case words contents of
      (val:_) -> return $ readMaybe val
      _ -> return Nothing
  ) $ \(_ :: SomeException) -> return Nothing
