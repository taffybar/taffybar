{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.Information.Brightness (readBrightnessValues) where

import System.UDev.Context
import System.UDev.Device
import System.UDev.Monitor

import System.UDev.Enumerate
import System.UDev.List

import Control.Concurrent (Chan, newChan, writeChan, threadWaitRead)

import Control.Monad (when)

import Data.ByteString (ByteString())

import System.Taffybar.Util (backgroundLoop)
import System.Taffybar.Util.ReadByteString (parseInt)

--Only returns the first match
scanForDeviceBySubsystem :: UDev -> Subsystem -> IO Device
scanForDeviceBySubsystem udev subSysName = do
  e <- newEnumerate udev
  addMatchSubsystem e subSysName
  scanDevices e 
  Just ls <- getListEntry e
  path <- getName ls
  newFromSysPath udev path
  
getInt :: ByteString -> Device -> IO Int
getInt name dev = parseInt <$> getSysattrValue dev name

readBrightnessValues :: IO (Chan Double)
readBrightnessValues = withUDev $ \udev -> do
  max_brightness <- scanForDeviceBySubsystem udev "backlight" >>= getInt "max_brightness"
  monitor <- newFromNetlink udev UDevId
  -- Uncomment this when next version of udev is released:
  -- filterAddMatchSubsystemDevtype monitor "backlight" Nothing
  enableReceiving monitor
  fd <- getFd monitor
  chan <- newChan
  backgroundLoop $ do
    threadWaitRead fd
    dev <- receiveDevice monitor
    -- replace this guard with above filterAddMatchSubsystemDevtype when next udev is released
    when (getSubsystem dev == Just "backlight") $ do
      actual_brightness <- getInt "actual_brightness" dev
      let percent = fromIntegral actual_brightness / fromIntegral max_brightness
      writeChan chan percent
  return chan

