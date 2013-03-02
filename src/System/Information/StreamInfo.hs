--------------------------------------------------------------------------------
-- |
-- Module      : System.Information.StreamInfo
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Generic code to poll any of the many data files maintained by the kernel in
-- POSIX systems. Provides methods for applying a custom parsing function to the
-- contents of the file and to calculate differentials across one or more values
-- provided via the file.
--
--------------------------------------------------------------------------------

module System.Information.StreamInfo
    ( getParsedInfo
    , getLoad
    , getAccLoad
    , getTransfer
    ) where

import Control.Concurrent (threadDelay)
import Data.IORef
import Data.Maybe (fromJust)

-- | Apply the given parser function to the file under the given path to produce
-- a lookup map, then use the given selector as key to extract from it the
-- desired value.
getParsedInfo :: FilePath -> (String -> [(String, [Integer])]) -> String -> IO [Integer]
getParsedInfo path parser selector = do
    file <- readFile path
    (length file) `seq` return ()
    return (fromJust $ lookup selector $ parser file)

truncVal :: Double -> Double
truncVal v
  | isNaN v || v < 0.0 = 0.0
  | otherwise = v

-- | Convert the given list of Integer to a list of the ratios of each of its
-- elements against their sum.
toRatioList :: [Integer] -> [Double]
toRatioList deltas = map truncVal ratios
    where total = fromIntegral $ foldr (+) 0 deltas
          ratios = map ((/total) . fromIntegral) deltas

-- | Execute the given action twice with the given delay in-between and return
-- the difference between the two samples.
probe :: IO [Integer] -> Double -> IO [Integer]
probe action delay = do
    a <- action
    threadDelay $ round (delay * 1e6)
    b <- action
    return $ zipWith (-) b a

-- | Execute the given action once and return the difference between the
-- obtained sample and the one contained in the given IORef.
accProbe :: IO [Integer] -> IORef [Integer] -> IO [Integer]
accProbe action sample = do
    a <- readIORef sample
    b <- action
    writeIORef sample b
    return $ zipWith (-) b a

-- | Probe the given action and, interpreting the result as a variation in time,
-- return the speed of change of its values.
getTransfer :: Double -> IO [Integer] -> IO [Double]
getTransfer interval action = do
    deltas <- probe action interval
    return $ map (truncVal . (/interval) . fromIntegral) deltas

-- | Probe the given action and return the relative variation of each of the
-- obtained values against the whole, where the whole is calculated as the sum
-- of all the values in the probe.
getLoad :: Double -> IO [Integer] -> IO [Double] 
getLoad interval action = do
    deltas <- probe action interval
    return $ toRatioList deltas

-- | Similar to getLoad, but execute the given action only once and use the
-- given IORef to calculate the result and to save the current value, so it
-- can be reused in the next call.
getAccLoad :: IORef [Integer] -> IO [Integer] -> IO [Double]
getAccLoad sample action = do
     deltas <- accProbe action sample
     return $ toRatioList deltas

