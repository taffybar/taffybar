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
  defaultNetFormat
  ) where

import           Data.IORef
import           Graphics.UI.Gtk
import           System.Information.Network           (getNetInfo)
import           System.Taffybar.Widgets.PollingLabel
import           Text.Printf                          (printf)
import           Text.StringTemplate

defaultNetFormat :: String
defaultNetFormat = "▼ $inKB$kb/s ▲ $outKB$kb/s"

-- | Creates a new network monitor widget. It consists of two 'PollingLabel's,
-- one for incoming and one for outgoing traffic fed by regular calls to
-- 'getNetInfo'.
netMonitorNew :: Double -- ^ Polling interval (in seconds, e.g. 1.5)
              -> String -- ^ Name of the network interface to monitor (e.g. \"eth0\", \"wlan1\")
              -> IO Widget
netMonitorNew interval interface =
  netMonitorNewWith interval interface 2 defaultNetFormat

-- | Creates a new network monitor widget with custom template and precision.
-- Similar to 'netMonitorNew'.
--
-- The format template currently supports three units: bytes,
-- kilobytes, and megabytes.  Automatic intelligent unit selection is
-- planned, eventually.
netMonitorNewWith :: Double -- ^ Polling interval (in seconds, e.g. 1.5)
                  -> String -- ^ Name of the network interface to monitor (e.g. \"eth0\", \"wlan1\")
                  -> Integer -- ^ Precision for an output
                  -> String -- ^ Template for an output. You can use variables: $inB$, $inKB$, $inMB$, $outB$, $outKB$, $outMB$
                  -> IO Widget
netMonitorNewWith interval interface prec template = do
    sample <- newIORef [0, 0]
    label  <- pollingLabelNew "" interval $ showInfo sample interval interface template prec
    widgetShowAll label
    return $ toWidget label

showInfo :: IORef [Integer] -> Double -> String -> String -> Integer -> IO String
showInfo sample interval interface template prec = do
    maybeThisSample <- getNetInfo interface
    case maybeThisSample of
      Nothing -> return ""
      Just thisSample -> do
        lastSample <- readIORef sample
        writeIORef sample thisSample
        let deltas = map (max 0 . fromIntegral) $ zipWith (-) thisSample lastSample
            speed@[incomingb, outgoingb] = map (/(interval)) deltas
            [incomingkb, outgoingkb] = map (setDigits prec . (/1024)) speed
            [incomingmb, outgoingmb] = map (setDigits prec . (/square 1024)) speed
            attribs = [ ("inB", show incomingb)
                      , ("inKB", incomingkb)
                      , ("inMB", incomingmb)
                      , ("outB", show outgoingb)
                      , ("outKB", outgoingkb)
                      , ("outMB", outgoingmb)
                      ]
        return . render . setManyAttrib attribs $ newSTMP template

square :: Double -> Double
square x = x ^ (2 :: Int)

setDigits :: Integer -> Double -> String
setDigits dig a = printf format a
    where format = "%." ++ show dig ++ "f"
