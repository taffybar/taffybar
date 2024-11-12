{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.DBus.Toggle
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides a dbus interface that allows users to toggle the display
-- of taffybar on each monitor while it is running.
-----------------------------------------------------------------------------

module System.Taffybar.DBus.Toggle ( handleDBusToggles ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           DBus
import           DBus.Client
import           Data.Int
import qualified Data.Map as M
import           Data.Maybe
import qualified GI.Gdk as Gdk
import           Graphics.UI.GIGtkStrut
import           System.Directory
import           System.FilePath.Posix
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.Util
import           Text.Printf
import           Text.Read ( readMaybe )
import qualified UnliftIO.MVar as MV
import           UnliftIO.Exception (SomeException, catch)

-- $usage
--
-- To use this module, import it in your taffybar.hs and wrap your config with
-- the 'handleDBusToggles' function:
--
-- > main = dyreTaffybar $ handleDBusToggles myConfig
--
-- To toggle taffybar on the monitor that is currently active, issue the
-- following command:
--
-- > dbus-send --print-reply=literal --dest=taffybar.toggle /taffybar/toggle taffybar.toggle.toggleCurrent

logIO :: System.Log.Logger.Priority -> String -> IO ()
logIO = logM "System.Taffybar.DBus.Toggle"

logT :: MonadIO m => System.Log.Logger.Priority -> String -> m ()
logT p = liftIO . logIO p

getActiveMonitorNumber :: MaybeT IO Int
getActiveMonitorNumber = do
  display <- MaybeT Gdk.displayGetDefault
  seat <- lift $ Gdk.displayGetDefaultSeat display
  device <- MaybeT $ Gdk.seatGetPointer seat
  lift $ do
    (_, x, y) <- Gdk.deviceGetPosition device
    Gdk.displayGetMonitorAtPoint display x y >>= getMonitorNumber

getMonitorNumber :: Gdk.Monitor -> IO Int
getMonitorNumber monitor = do
  display <- Gdk.monitorGetDisplay monitor
  monitorCount <- Gdk.displayGetNMonitors display
  monitors <- mapM (Gdk.displayGetMonitor display) [0..(monitorCount-1)]
  monitorGeometry <- Gdk.getMonitorGeometry monitor
  let equalsMonitor (Just other, _) =
        do
          otherGeometry <- Gdk.getMonitorGeometry other
          case (otherGeometry, monitorGeometry) of
               (Nothing, Nothing) -> return True
               (Just g1, Just g2) -> Gdk.rectangleEqual g1 g2
               _ -> return False
      equalsMonitor _ = return False
  snd . fromMaybe (Nothing, 0) . listToMaybe <$>
      filterM equalsMonitor (zip monitors [0..])

taffybarTogglePath :: ObjectPath
taffybarTogglePath = "/taffybar/toggle"

taffybarToggleInterface :: InterfaceName
taffybarToggleInterface = "taffybar.toggle"

toggleStateFile :: IO FilePath
toggleStateFile = (</> "toggle_state.dat") <$> taffyStateDir

newtype TogglesMVar = TogglesMVar (MV.MVar (M.Map Int Bool))

getTogglesVar :: TaffyIO TogglesMVar
getTogglesVar = getStateDefault $ lift (TogglesMVar <$> MV.newMVar M.empty)

toggleBarConfigGetter :: BarConfigGetter -> BarConfigGetter
toggleBarConfigGetter getConfigs = do
  barConfigs <- getConfigs
  TogglesMVar enabledVar <- getTogglesVar
  numToEnabled <- lift $ MV.readMVar enabledVar
  let isEnabled monNumber = fromMaybe True $ M.lookup monNumber numToEnabled
      isConfigEnabled =
        isEnabled . fromIntegral . fromMaybe 0 . strutMonitor . strutConfig
  return $ filter isConfigEnabled barConfigs

exportTogglesInterface :: TaffyIO ()
exportTogglesInterface = do
  TogglesMVar enabledVar <- getTogglesVar
  ctx <- ask
  lift $ taffyStateDir >>= createDirectoryIfMissing True
  stateFile <- lift toggleStateFile
  let toggleTaffyOnMon fn mon = runTaffy ctx $ do
        lift $ MV.modifyMVar_ enabledVar $ \numToEnabled -> do
          let current = fromMaybe True $ M.lookup mon numToEnabled
              result = M.insert mon (fn current) numToEnabled
          logIO DEBUG $ printf "Toggle state before: %s, after %s"
                  (show numToEnabled) (show result)
          catch (writeFile stateFile (show result)) $ \e ->
            logIO WARNING $ printf "Unable to write to toggle state file %s, error: %s"
                  (show stateFile) (show (e :: SomeException))
          return result
        refreshTaffyWindows
      toggleTaffy = do
        num <- runMaybeT getActiveMonitorNumber
        toggleTaffyOnMon not $ fromMaybe 0 num
      takeInt :: (Int -> a) -> (Int32 -> a)
      takeInt = (. fromIntegral)
  client <- asks sessionDBusClient
  let interface =
        defaultInterface
        { interfaceName = taffybarToggleInterface
        , interfaceMethods =
          [ autoMethod "toggleCurrent" toggleTaffy
          , autoMethod "toggleOnMonitor" $ takeInt $ toggleTaffyOnMon not
          , autoMethod "hideOnMonitor" $
            takeInt $ toggleTaffyOnMon (const False)
          , autoMethod "showOnMonitor" $
            takeInt $ toggleTaffyOnMon (const True)
          , autoMethod "refresh" $ runTaffy ctx refreshTaffyWindows
          , autoMethod "exit" $ exitTaffybar ctx
          ]
        }
  lift $ do
    _ <- requestName client "taffybar.toggle"
       [nameAllowReplacement, nameReplaceExisting]
    export client taffybarTogglePath interface

dbusTogglesStartupHook :: TaffyIO ()
dbusTogglesStartupHook = do
  TogglesMVar enabledVar <- getTogglesVar
  logT DEBUG "Loading toggle state"
  lift $ do
    stateFilepath <- toggleStateFile
    filepathExists <- doesFileExist stateFilepath
    mStartingMap <-
      if filepathExists
      then
        readMaybe <$> readFile stateFilepath
      else
        return Nothing
    MV.modifyMVar_ enabledVar $ const $ return $ fromMaybe M.empty mStartingMap
  logT DEBUG "Exporting toggles interface"
  exportTogglesInterface

handleDBusToggles :: TaffybarConfig -> TaffybarConfig
handleDBusToggles config =
  config { getBarConfigsParam =
             toggleBarConfigGetter $ getBarConfigsParam config
         , startupHook = startupHook config >> dbusTogglesStartupHook
         }
