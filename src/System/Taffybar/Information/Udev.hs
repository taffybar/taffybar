{-# LANGUAGE ForeignFunctionInterface #-}

-- | Minimal libudev bindings used for event-driven backlight monitoring.
--
-- We only bind what we need for Waybar-style monitoring:
-- 1. Subscribe to udev netlink events for the "backlight" subsystem.
-- 2. Expose the monitor fd so callers can block (with a timeout) using
--    'GHC.Conc.threadWaitRead'.
-- 3. Drain pending udev events via 'udev_monitor_receive_device'.
module System.Taffybar.Information.Udev
  ( UdevBacklightMonitor,
    openBacklightMonitor,
    closeBacklightMonitor,
    backlightMonitorFd,
    drainBacklightMonitor,
  )
where

import Control.Exception (bracketOnError, throwIO)
import Control.Monad (void, when)
import Foreign hiding (void)
import Foreign.C
import System.Posix.Types (Fd (..))

data Udev

data UdevMonitor

data UdevDevice

foreign import ccall unsafe "udev_new"
  c_udev_new :: IO (Ptr Udev)

foreign import ccall unsafe "udev_unref"
  c_udev_unref :: Ptr Udev -> IO (Ptr Udev)

foreign import ccall unsafe "udev_monitor_new_from_netlink"
  c_udev_monitor_new_from_netlink :: Ptr Udev -> CString -> IO (Ptr UdevMonitor)

foreign import ccall unsafe "udev_monitor_unref"
  c_udev_monitor_unref :: Ptr UdevMonitor -> IO (Ptr UdevMonitor)

foreign import ccall unsafe "udev_monitor_filter_add_match_subsystem_devtype"
  c_udev_monitor_filter_add_match_subsystem_devtype ::
    Ptr UdevMonitor ->
    CString ->
    CString -> -- nullable
    IO CInt

foreign import ccall unsafe "udev_monitor_enable_receiving"
  c_udev_monitor_enable_receiving :: Ptr UdevMonitor -> IO CInt

foreign import ccall unsafe "udev_monitor_get_fd"
  c_udev_monitor_get_fd :: Ptr UdevMonitor -> IO CInt

foreign import ccall unsafe "udev_monitor_receive_device"
  c_udev_monitor_receive_device :: Ptr UdevMonitor -> IO (Ptr UdevDevice)

foreign import ccall unsafe "udev_device_unref"
  c_udev_device_unref :: Ptr UdevDevice -> IO (Ptr UdevDevice)

data UdevBacklightMonitor = UdevBacklightMonitor
  { monitorUdev :: Ptr Udev,
    monitorHandle :: Ptr UdevMonitor,
    monitorFd :: Fd
  }

openBacklightMonitor :: IO UdevBacklightMonitor
openBacklightMonitor = do
  udev <- c_udev_new
  whenNull udev "udev_new failed"
  bracketOnError (pure udev) (void . c_udev_unref) $ \udev' ->
    withCString "udev" $ \netlinkName -> do
      mon <- c_udev_monitor_new_from_netlink udev' netlinkName
      whenNull mon "udev_monitor_new_from_netlink failed"
      bracketOnError (pure mon) (void . c_udev_monitor_unref) $ \mon' -> do
        withCString "backlight" $ \subsystem -> do
          rc <- c_udev_monitor_filter_add_match_subsystem_devtype mon' subsystem nullPtr
          whenNeg rc "udev_monitor_filter_add_match_subsystem_devtype failed"
        rc2 <- c_udev_monitor_enable_receiving mon'
        whenNeg rc2 "udev_monitor_enable_receiving failed"
        fdC <- c_udev_monitor_get_fd mon'
        whenNeg fdC "udev_monitor_get_fd failed"
        pure $
          UdevBacklightMonitor
            { monitorUdev = udev',
              monitorHandle = mon',
              monitorFd = Fd fdC
            }
  where
    whenNull p msg = when (p == nullPtr) $ throwIO (userError msg)
    whenNeg rc msg = when (rc < 0) $ throwIO (userError msg)

closeBacklightMonitor :: UdevBacklightMonitor -> IO ()
closeBacklightMonitor mon = do
  void $ c_udev_monitor_unref (monitorHandle mon)
  void $ c_udev_unref (monitorUdev mon)

backlightMonitorFd :: UdevBacklightMonitor -> Fd
backlightMonitorFd = monitorFd

-- | Drain pending udev events from the monitor.
--
-- Call this after the monitor fd becomes readable.
drainBacklightMonitor :: UdevBacklightMonitor -> IO ()
drainBacklightMonitor mon = do
  -- Only consume a single event per wake; if multiple are queued, the fd will
  -- stay readable and we'll be woken again.
  dev <- c_udev_monitor_receive_device (monitorHandle mon)
  if dev == nullPtr
    then pure ()
    else void $ c_udev_device_unref dev
