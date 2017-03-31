{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
import qualified Data.Traversable as T
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
  interfaceRefs <- T.forM interfaces $ \i -> (i,) <$> newIORef (0, 0)
  let showResult = showInfo template prec <$> calculateNetUse interfaceRefs
  label <- pollingLabelNew "" interval showResult
  widgetShowAll label
  return (toWidget label)
  where
    calculateNetUse ifaceRefs = do
      mIfaceInfos <- T.forM ifaceRefs $ \(i, ref) -> do
        mIfaceInfo <- getNetInfo i
        return $ fmap (\ifaceInfo -> (ref, ifaceInfo)) mIfaceInfo
      speeds <- T.forM (catMaybes mIfaceInfos) $ \(ref, ifaceInfo) -> do
        let ii = case ifaceInfo of
              [info1, info2] -> (info1, info2)
              _ -> (0, 0)
        calcSpeed interval ref ii
      return $ foldr (\(d, u) (dsum, usum) -> (dsum + d, usum + u)) (0, 0) speeds

calcSpeed :: Double -> IORef (Int, Int) -> (Int, Int) -> IO (Double, Double)
calcSpeed interval sample result@(r1, r2) = do
    (s1, s2) <- readIORef sample
    writeIORef sample result
    return (max 0 (fromIntegral (r1 - s1) / interval), max 0 (fromIntegral (r2 - s2) / interval))

showInfo :: String -> Int -> (Double, Double) -> String
showInfo template prec (incomingb, outgoingb) =
  let
    attribs = [ ("inB", show incomingb)
              , ("inKB", toKB prec incomingb)
              , ("inMB", toMB prec incomingb)
              , ("outB", show outgoingb)
              , ("outKB", toKB prec outgoingb)
              , ("outMB", toMB prec outgoingb)
              ]
  in
    render . setManyAttrib attribs $ newSTMP template

toKB :: Int -> Double -> String
toKB prec = setDigits prec . (/1024)

toMB :: Int -> Double -> String
toMB prec = setDigits prec . (/ (1024 * 1024))

setDigits :: Int -> Double -> String
setDigits dig a = printf format a
    where format = "%." ++ show dig ++ "f"


