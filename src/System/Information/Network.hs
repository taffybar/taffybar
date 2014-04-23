-----------------------------------------------------------------------------
-- |
-- Module      : System.Information.Network
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Provides information about network traffic over selected interfaces,
-- obtained from parsing the @\/proc\/net\/dev@ file using some of the
-- facilities provided by the "System.Information.StreamInfo" module.
--
-----------------------------------------------------------------------------

module System.Information.Network ( getNetInfo ) where

import Control.Applicative
import Data.Maybe ( mapMaybe )
import Safe ( atMay, initSafe, readDef )
import System.Information.StreamInfo ( getParsedInfo )

-- | Returns a two-element list containing the current number of bytes
-- received and transmitted via the given network interface (e.g. \"wlan0\"),
-- according to the contents of the @\/proc\/dev\/net@ file.
getNetInfo :: String -> IO (Maybe [Integer])
getNetInfo iface = do
  isUp <- isInterfaceUp iface
  case isUp of
    True -> Just <$> getParsedInfo "/proc/net/dev" parse iface
    False -> return Nothing

parse :: String -> [(String, [Integer])]
parse = mapMaybe tuplize . map words . drop 2 . lines

tuplize :: [String] -> Maybe (String, [Integer])
tuplize s = do
  dev <- initSafe <$> s `atMay` 0
  down <- readDef (-1) <$> s `atMay` 1
  up <- readDef (-1) <$> s `atMay` out
  return (dev, [down, up])
  where
    out = (length s) - 8

isInterfaceUp :: String -> IO Bool
isInterfaceUp iface = do
  state <- readFile $ "/sys/class/net/" ++ iface ++ "/operstate"
  case state of
    'u' : _ -> return True
    _ -> return False
