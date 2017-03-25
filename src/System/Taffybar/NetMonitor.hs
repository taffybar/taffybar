{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.NetMonitor
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple text widget that displays incoming\/outgoing network traffic over
-- one selected interface, as provided by the "System.Information.Network"
-- module.
--
-----------------------------------------------------------------------------

module System.Taffybar.NetMonitor (
  netMonitorNew,
  netMonitorNewWith,
  netMonitorMultiNew,
  netMonitorMultiNewWith,
  defaultNetFormat
  ) where

import           Data.IORef
import           Graphics.UI.Gtk
import           System.Information.Network           (getNetInfo)
import           System.Taffybar.Widgets.PollingLabel
import           Text.Printf                          (printf)
import           Text.StringTemplate
import           Data.Maybe                           (catMaybes)
import           Data.Traversable                     (traverse)
import           Control.Applicative                     ((<$>))

defaultNetFormat :: String
defaultNetFormat = "▼ $inKB$kb/s ▲ $outKB$kb/s"

-- | Creates a new network monitor widget. It consists of two 'PollingLabel's,
-- one for incoming and one for outgoing traffic fed by regular calls to
-- 'getNetInfo'.
netMonitorNew :: Double -- ^ Polling interval (in seconds, e.g. 1.5)
              -> String -- ^ Name of the network interface to monitor (e.g. \"eth0\", \"wlan1\")
              -> IO Widget
netMonitorNew interval interface = netMonitorMultiNew interval [interface]

-- | Creates a new network monitor widget with custom template and precision.
-- Similar to 'netMonitorNew'.
--
-- The format template currently supports three units: bytes,
-- kilobytes, and megabytes.  Automatic intelligent unit selection is
-- planned, eventually.
netMonitorNewWith :: Double -- ^ Polling interval (in seconds, e.g. 1.5)
                  -> String -- ^ Name of the network interface to monitor (e.g. \"eth0\", \"wlan1\")
                  -> Int -- ^ Precision for an output
                  -> String -- ^ Template for an output. You can use variables: $inB$, $inKB$, $inMB$, $outB$, $outKB$, $outMB$
                  -> IO Widget
netMonitorNewWith interval interface prec template = netMonitorMultiNewWith interval [interface] prec template

-- | Like `netMonitorNew` but allows specification of multiple interfaces.
--   Interfaces are allowed to not exist at all (e.g. unplugged usb ethernet),
--   the resulting speed is the speed of all available interfaces summed up. So
--   you get your network speed regardless of which interface you are currently
--   using.
netMonitorMultiNew :: Double -- ^ Polling interval (in seconds, e.g. 1.5)
              -> [String] -- ^ Name of the network interfaces to monitor (e.g. \"eth0\", \"wlan1\")
              -> IO Widget
netMonitorMultiNew interval interfaces = netMonitorMultiNewWith interval interfaces 2 defaultNetFormat

-- | Like `newMonitorNewWith` but for multiple interfaces.
netMonitorMultiNewWith :: Double -- ^ Polling interval (in seconds, e.g. 1.5)
                  -> [String] -- ^ Name of the network interfaces to monitor (e.g. \"eth0\", \"wlan1\")
                  -> Int -- ^ Precision for an output
                  -> String -- ^ Template for an output. You can use variables: $inB$, $inKB$, $inMB$, $outB$, $outKB$, $outMB$
                  -> IO Widget
netMonitorMultiNewWith interval interfaces prec template = do
    refs :: [IORef [Int]] <- traverse (const $ newIORef [0,0]) interfaces
    let
      calcResult = do
        mInfos :: [Maybe [Int]] <- traverse getNetInfo interfaces
        let
          results :: [(IORef [Int], [Int])]
          results = catMaybes . fmap sequenceMaybePair $ zip refs mInfos
        speeds <- traverse (uncurry $ calcSpeed interval) results
        return $ foldr (\[d,u] [dsum,usum] -> [dsum + d, usum + u]) [0,0] speeds

      showResult = showInfo template prec <$> calcResult

    label  <- pollingLabelNew "" interval $ showResult
    widgetShowAll label
    return $ toWidget label

calcSpeed :: Double -> IORef [Int] -> [Int] -> IO [Double]
calcSpeed interval sample result = do
    lastSample <- readIORef sample
    writeIORef sample result
    let deltas = map (max 0 . fromIntegral) $ zipWith (-) result lastSample
    return $ map (/interval) deltas

showInfo :: String -> Int -> [Double] -> String
showInfo template prec speed@[incomingb, outgoingb] =
  let
    [incomingkb, outgoingkb] = map (setDigits prec . (/1024)) speed
    [incomingmb, outgoingmb] = map (setDigits prec . (/square 1024)) speed
    attribs = [ ("inB", show incomingb)
              , ("inKB", incomingkb)
              , ("inMB", incomingmb)
              , ("outB", show outgoingb)
              , ("outKB", outgoingkb)
              , ("outMB", outgoingmb)
              ]
  in
    render . setManyAttrib attribs $ newSTMP template

square :: Double -> Double
square x = x ^ (2 :: Int)

setDigits :: Int -> Double -> String
setDigits dig a = printf format a
    where format = "%." ++ show dig ++ "f"

-- Needed for ghc-7.8:
sequenceMaybePair :: (a, Maybe b) -> Maybe (a,b)
sequenceMaybePair (_, Nothing) = Nothing
sequenceMaybePair (a, Just b) = Just (a, b)


