{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.ToggleMonitor
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides a dbus interface that allows users to toggle the display
-- of taffybar on each monitor while it is running.

module System.Taffybar.ToggleMonitor (
  handleToggleRequests,
  toggleableMonitors,
  withToggleSupport
) where

import           Control.Applicative
import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           DBus
import           DBus.Client
import           Data.Int
import qualified Data.Map as M
import           Data.Maybe
import           Graphics.UI.Gtk.Gdk.Screen
import           Paths_taffybar ( getDataDir )
import           Prelude
import           System.Directory
import           System.FilePath.Posix
import           System.Taffybar
import           Text.Read ( readMaybe )

-- $usage
--
-- To use this module, import it in your taffybar.hs and use the
-- 'withToggleSupport' function to start taffybar, where you might otherwise
-- have used 'defaultTaffybar', like so:
--
-- > main = withToggleSupport defaultTaffybarConfig {}
--
-- To toggle taffybar on the monitor that is currently active, issue the
-- following command:
--
-- > dbus-send --print-reply=literal --dest=taffybar.toggle /taffybar/toggle taffybar.toggle.toggleCurrent


toggleableMonitors
  :: MV.MVar (M.Map Int Bool)
  -> TaffybarConfigEQ
  -> IO (Int -> Maybe TaffybarConfigEQ)
toggleableMonitors enabledVar cfg = do
  numToEnabled <- MV.readMVar enabledVar
  let fn monNumber =
        if fromMaybe True $ M.lookup monNumber numToEnabled
        then Just cfg
        else Nothing
  return fn

getActiveScreenNumber :: MaybeT IO Int
getActiveScreenNumber = do
  screen <- MaybeT screenGetDefault
  window <- MaybeT $ screenGetActiveWindow screen
  lift $ screenGetMonitorAtWindow screen window

taffybarTogglePath :: ObjectPath
taffybarTogglePath = "/taffybar/toggle"

taffybarToggleInterface :: InterfaceName
taffybarToggleInterface = "taffybar.toggle"

toggleStateFile :: IO FilePath
toggleStateFile = (</> "toggleState.hs") <$> getDataDir

handleToggleRequests :: MV.MVar (M.Map Int Bool) -> IO () -> IO ()
handleToggleRequests enabledVar refreshTaffyWindows = do
  let toggleTaffyOnMon fn mon = do
        MV.modifyMVar_ enabledVar $ \numToEnabled -> do
          let current = fromMaybe True $ M.lookup mon numToEnabled
              result = M.insert mon (fn current) numToEnabled
          flip writeFile (show result) =<< toggleStateFile
          return result
        refreshTaffyWindows
      toggleTaffy = do
        num <- runMaybeT getActiveScreenNumber
        toggleTaffyOnMon not $ fromMaybe 0 num
      makeMethod :: AutoMethod fn => MemberName -> fn -> Method
      makeMethod = autoMethod taffybarToggleInterface
      takeInt :: (Int -> a) -> (Int32 -> a)
      takeInt = (. fromIntegral)
  client <- connectSession
  _ <- requestName client "taffybar.toggle"
       [nameAllowReplacement, nameReplaceExisting]
  export client taffybarTogglePath
           [ makeMethod "toggleCurrent" toggleTaffy
           , makeMethod "toggleOnMonitor" $ takeInt $ toggleTaffyOnMon not
           , makeMethod "hideOnMonitor" $
             takeInt $ toggleTaffyOnMon (const False)
           , makeMethod "showOnMonitor" $
             takeInt $ toggleTaffyOnMon (const True)]

withToggleSupport :: TaffybarConfig -> IO ()
withToggleSupport config = do
  stateFilepath <- toggleStateFile
  filepathExists <- doesFileExist stateFilepath
  startingMap <-
    if filepathExists
    then
      readMaybe <$> readFile stateFilepath
    else
      return Nothing
  enabledVar <- MV.newMVar $ fromMaybe M.empty startingMap
  let modified = config { startRefresher = handleToggleRequests enabledVar
                        , getMonitorConfig = toggleableMonitors enabledVar
                        }
  defaultTaffybar modified
