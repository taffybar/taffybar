-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.Network
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Provides information about network traffic over selected interfaces,
-- obtained from parsing the @\/proc\/net\/dev@ file using some of the
-- facilities provided by the "System.Taffybar.Information.StreamInfo" module.
module System.Taffybar.Information.Network where

import qualified Control.Concurrent.MVar as MV
import Control.Exception (SomeException, catch)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Maybe (mapMaybe)
import Data.Time.Clock
import Data.Time.Clock.System
import Safe (atMay, initSafe, readDef)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.StreamInfo (getParsedInfo)
import System.Taffybar.Information.Wakeup (taffyForeverWithDelay)

-- | Source file for Linux per-interface byte counters.
networkInfoFile :: FilePath
networkInfoFile = "/proc/net/dev"

-- | Returns a two-element list containing the current number of bytes received
-- and transmitted via the given network interface (e.g. \"wlan0\"), according
-- to the contents of the @\/proc\/dev\/net@ file.
getNetInfo :: String -> IO (Maybe [Int])
getNetInfo iface = runMaybeT $ do
  isInterfaceUp iface
  handleFailure $ getParsedInfo networkInfoFile parseDevNet' iface

-- | Parse @/proc/net/dev@ into a simplified map of
-- @(interface, [receivedBytes, transmittedBytes])@.
parseDevNet' :: String -> [(String, [Int])]
parseDevNet' input =
  map makeList $ parseDevNet input
  where
    makeList (a, (u, d)) = (a, [u, d])

-- | Parse @/proc/net/dev@ into @(interface, (receivedBytes, transmittedBytes))@.
parseDevNet :: String -> [(String, (Int, Int))]
parseDevNet = mapMaybe (getDeviceUpDown . words) . drop 2 . lines

-- | Parse one tokenized @/proc/net/dev@ line.
getDeviceUpDown :: [String] -> Maybe (String, (Int, Int))
getDeviceUpDown s = do
  dev <- initSafe <$> s `atMay` 0
  down <- readDef (-1) <$> s `atMay` 1
  up <- readDef (-1) <$> s `atMay` out
  dev `seq` down `seq` up `seq` return (dev, (down, up))
  where
    out = length s - 8

-- Nothing if interface does not exist or is down

-- | Check whether a network interface exists and reports as up.
isInterfaceUp :: String -> MaybeT IO ()
isInterfaceUp iface = do
  state <- handleFailure $ readFile $ "/sys/class/net/" ++ iface ++ "/operstate"
  case state of
    'u' : _ -> return ()
    _ -> mzero

-- | Convert IO exceptions into @Nothing@ for polling helpers.
handleFailure :: IO a -> MaybeT IO a
handleFailure action = MaybeT $ catch (Just <$> action) eToNothing
  where
    eToNothing :: SomeException -> IO (Maybe a)
    eToNothing _ = pure Nothing

-- | Collect per-interface byte-counter samples with timestamps.
getDeviceSamples :: IO (Maybe [TxSample])
getDeviceSamples = runMaybeT $ handleFailure $ do
  contents <- readFile networkInfoFile
  length contents `seq` return ()
  time <- liftIO getSystemTime
  let mkSample (device, (up, down)) =
        TxSample
          { sampleUp = up,
            sampleDown = down,
            sampleTime = time,
            sampleDevice = device
          }
  return $ map mkSample $ parseDevNet contents

-- | Raw network sample for a single interface at a point in time.
data TxSample = TxSample
  { sampleUp :: Int,
    sampleDown :: Int,
    sampleTime :: SystemTime,
    sampleDevice :: String
  }
  deriving (Show, Eq)

-- | Periodically monitor interface speeds and call a callback with
-- per-interface download/upload rates in bytes per second.
monitorNetworkInterfaces ::
  (RealFrac a1) =>
  a1 -> ([(String, (Rational, Rational))] -> IO ()) -> TaffyIO ()
monitorNetworkInterfaces interval onUpdate = do
  samplesVar <- liftIO $ MV.newMVar []
  let sampleToSpeeds (device, (s1, s2)) = (device, getSpeed s1 s2)
      doOnUpdate samples = do
        let speedInfo = map sampleToSpeeds samples
        onUpdate speedInfo
        return samples
      doUpdate = MV.modifyMVar_ samplesVar (updateSamples >=> doOnUpdate)
  void $ taffyForeverWithDelay interval (liftIO doUpdate)

-- | Update sample history by pairing newest and previous samples per interface.
updateSamples ::
  [(String, (TxSample, TxSample))] ->
  IO [(String, (TxSample, TxSample))]
updateSamples currentSamples = do
  let getLast sample@TxSample {sampleDevice = device} =
        maybe sample fst $ lookup device currentSamples
      getSamplePair sample@TxSample {sampleDevice = device} =
        let lastSample = getLast sample
         in lastSample `seq` (device, (sample, lastSample))
  maybe currentSamples (map getSamplePair) <$> getDeviceSamples

-- | Compute download/upload speeds from two samples.
getSpeed :: TxSample -> TxSample -> (Rational, Rational)
getSpeed
  TxSample
    { sampleUp = thisUp,
      sampleDown = thisDown,
      sampleTime = thisTime
    }
  TxSample
    { sampleUp = lastUp,
      sampleDown = lastDown,
      sampleTime = lastTime
    } =
    let intervalDiffTime =
          diffUTCTime
            (systemToUTCTime thisTime)
            (systemToUTCTime lastTime)
        intervalRatio =
          if intervalDiffTime == 0
            then 0
            else toRational $ 1 / intervalDiffTime
     in ( fromIntegral (thisDown - lastDown) * intervalRatio,
          fromIntegral (thisUp - lastUp) * intervalRatio
        )

-- | Sum download/upload speeds across interfaces.
sumSpeeds :: [(Rational, Rational)] -> (Rational, Rational)
sumSpeeds = foldr1 sumOne
  where
    sumOne (d1, u1) (d2, u2) = (d1 + d2, u1 + u2)
