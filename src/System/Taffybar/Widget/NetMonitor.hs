{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.NetMonitor
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple text widget that displays incoming\/outgoing network traffic over
-- one selected interface, as provided by the "System.Taffybar.Information.Network"
-- module.
--
-----------------------------------------------------------------------------

module System.Taffybar.Widget.NetMonitor
  {-# DEPRECATED "Use System.Taffybar.Widget.Text.NetworkMonitor instead" #-}
  ( defaultNetFormat
  , netMonitorMultiNew
  , netMonitorMultiNewWith
  , netMonitorNew
  , netMonitorNewWith
  , showInfo
  ) where

import           Control.Monad.Trans
import           Data.IORef
import           Data.Maybe (catMaybes)
import qualified Data.Traversable as T
import           Graphics.UI.Gtk
import           System.Taffybar.Information.Network (getNetInfo)
import           System.Taffybar.Widget.Generic.PollingLabel
import           System.Taffybar.Widget.Text.NetworkMonitor

-- | Creates a new network monitor widget. It consists of two 'PollingLabel's,
-- one for incoming and one for outgoing traffic fed by regular calls to
-- 'getNetInfo'.
netMonitorNew
  :: MonadIO m
  => Double -- ^ Polling interval (in seconds, e.g. 1.5)
  -> String -- ^ Name of the network interface to monitor (e.g. \"eth0\", \"wlan1\")
  -> m Widget
netMonitorNew interval interface = liftIO $ netMonitorMultiNew interval [interface]

-- | Creates a new network monitor widget with custom template and precision.
-- Similar to 'netMonitorNew'.
--
-- The format template currently supports four units: bytes,
-- kilobytes, megabytes, and auto.
netMonitorNewWith
  :: MonadIO m
  => Double -- ^ Polling interval (in seconds, e.g. 1.5)
  -> String -- ^ Name of the network interface to monitor (e.g. \"eth0\", \"wlan1\")
  -> Int -- ^ Precision for an output
  -> String -- ^ Template for an output. You can use variables: $inB$, $inKB$, $inMB$, $inAuto$, $outB$, $outKB$, $outMB$, $outAuto$
  -> m Widget
netMonitorNewWith interval interface prec template =
  liftIO $ netMonitorMultiNewWith interval [interface] prec template

-- | Like `netMonitorNew` but allows specification of multiple interfaces.
--   Interfaces are allowed to not exist at all (e.g. unplugged usb ethernet),
--   the resulting speed is the speed of all available interfaces summed up. So
--   you get your network speed regardless of which interface you are currently
--   using.
netMonitorMultiNew
  :: MonadIO m
  => Double -- ^ Polling interval (in seconds, e.g. 1.5)
  -> [String] -- ^ Name of the network interfaces to monitor (e.g. \"eth0\", \"wlan1\")
  -> m Widget
netMonitorMultiNew interval interfaces =
  liftIO $ netMonitorMultiNewWith interval interfaces 3 defaultNetFormat

-- | Like `newMonitorNewWith` but for multiple interfaces.
netMonitorMultiNewWith
  :: MonadIO m
  => Double -- ^ Polling interval (in seconds, e.g. 1.5)
  -> [String] -- ^ Name of the network interfaces to monitor (e.g. \"eth0\", \"wlan1\")
  -> Int -- ^ Precision for an output
  -> String -- ^ Template for an output. You can use variables: $inB$, $inKB$, $inMB$, $inAuto$, $outB$, $outKB$, $outMB$, $outAuto$
  -> m Widget
netMonitorMultiNewWith interval interfaces prec template = liftIO $ do
  interfaceRefs <- T.forM interfaces $ \i -> (i,) <$> newIORef (0, 0)
  let showResult = showInfo template prec <$> calculateNetUse interfaceRefs
  label <- pollingLabelNew "" interval showResult
  widgetShowAll label
  return (toWidget label)
  where
    calculateNetUse ifaceRefs = do
      mIfaceInfos <- T.forM ifaceRefs $ \(i, ref) ->
        fmap (\ifaceInfo -> (ref, ifaceInfo)) <$> getNetInfo i
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
