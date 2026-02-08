{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.KeyboardState
-- Copyright   : (c) Ivan Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Provides information about keyboard lock states (Caps Lock, Num Lock,
-- Scroll Lock) by reading LED brightness values from sysfs.
--
--------------------------------------------------------------------------------

module System.Taffybar.Information.KeyboardState
  ( KeyboardState(..)
  , getKeyboardState
  , defaultKeyboardState
  , findLedPath
  , defaultLedBasePath
  ) where

import Control.Exception (catch, SomeException)
import Data.List (find)
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>))

-- | Represents the state of keyboard lock keys.
data KeyboardState = KeyboardState
  { capsLock :: Bool
  , numLock :: Bool
  , scrollLock :: Bool
  } deriving (Eq, Show)

-- | Default keyboard state with all locks off.
defaultKeyboardState :: KeyboardState
defaultKeyboardState = KeyboardState False False False

-- | Default base path for LED sysfs entries.
defaultLedBasePath :: FilePath
defaultLedBasePath = "/sys/class/leds"

-- | Find the LED path for a given lock type by scanning sysfs.
-- Returns the full path to the brightness file, or Nothing if not found.
findLedPath :: FilePath -> String -> IO (Maybe FilePath)
findLedPath basePath lockType = do
  entries <- listDirectory basePath `catch` \(_ :: SomeException) -> return []
  let matchingEntry = find (matchesLockType lockType) entries
  case matchingEntry of
    Nothing -> return Nothing
    Just entry -> do
      let brightnessPath = basePath </> entry </> "brightness"
      exists <- doesFileExist brightnessPath
      return $ if exists then Just brightnessPath else Nothing

-- | Check if an LED entry name matches a lock type.
-- Matches patterns like "input15::capslock" for lock type "capslock".
matchesLockType :: String -> String -> Bool
matchesLockType lockType entry =
  ("::" ++ lockType) `isSuffixOf` entry || (":" ++ lockType) `isSuffixOf` entry
  where
    isSuffixOf :: String -> String -> Bool
    isSuffixOf suffix str = drop (length str - length suffix) str == suffix

-- | Read the brightness value from a sysfs LED file.
-- Returns True if the LED is on (brightness > 0), False otherwise.
readLedState :: FilePath -> IO Bool
readLedState path = do
  content <- readFile path `catch` \(_ :: SomeException) -> return "0"
  let value = reads (filter (/= '\n') content) :: [(Int, String)]
  case value of
    [(n, _)] -> return (n > 0)
    _ -> return False

-- | Get the current keyboard state by reading LED brightness from sysfs.
-- Uses the default LED base path ("/sys/class/leds").
-- Returns 'defaultKeyboardState' if LED files cannot be found.
getKeyboardState :: IO KeyboardState
getKeyboardState = getKeyboardStateFromPath defaultLedBasePath

-- | Get the keyboard state from a custom sysfs path.
getKeyboardStateFromPath :: FilePath -> IO KeyboardState
getKeyboardStateFromPath basePath = do
  capsPath <- findLedPath basePath "capslock"
  numPath <- findLedPath basePath "numlock"
  scrollPath <- findLedPath basePath "scrolllock"

  capsState <- maybe (return False) readLedState capsPath
  numState <- maybe (return False) readLedState numPath
  scrollState <- maybe (return False) readLedState scrollPath

  return KeyboardState
    { capsLock = capsState
    , numLock = numState
    , scrollLock = scrollState
    }
