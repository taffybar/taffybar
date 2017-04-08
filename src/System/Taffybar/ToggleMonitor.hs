{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.ToggleMonitor (
  handleToggleRequests,
  toggleableMonitors,
  withToggleSupport
) where

import qualified Control.Concurrent.MVar as MV
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           DBus
import           DBus.Client
import           Data.Int
import qualified Data.Map as M
import           Data.Maybe
import           Graphics.UI.Gtk.Gdk.Screen
import           System.Taffybar

toggleableMonitors :: MV.MVar (M.Map Int Bool)
                   -> TaffybarConfigEQ -> IO (Int -> Maybe TaffybarConfigEQ)
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

handleToggleRequests :: MV.MVar (M.Map Int Bool) -> IO () -> IO ()
handleToggleRequests enabledVar refreshTaffyWindows = do
  let toggleTaffyOnMon fn mon = do
        MV.modifyMVar_ enabledVar $ \numToEnabled -> do
          let current = fromMaybe True $ M.lookup mon numToEnabled
          return $ M.insert mon (fn current) numToEnabled
        refreshTaffyWindows
      toggleTaffy = do
        num <- runMaybeT getActiveScreenNumber
        toggleTaffyOnMon not $ fromMaybe 0 num
      makeMethod :: AutoMethod fn => MemberName -> fn -> Method
      makeMethod = autoMethod taffybarToggleInterface
      takeInt :: (Int -> a) -> (Int32 -> a)
      takeInt = (. fromIntegral)
  client <- connectSession
  _ <- requestName client "taffybar.toggle" [nameAllowReplacement, nameReplaceExisting]
  export client taffybarTogglePath $
           [ makeMethod "toggleCurrent" $ toggleTaffy
           , makeMethod "toggleOnMonitor" $ takeInt $ toggleTaffyOnMon not
           , makeMethod "hideOnMonitor" $ takeInt $ toggleTaffyOnMon (const False)
           , makeMethod "showOnMonitor" $ takeInt $ toggleTaffyOnMon (const True)]

withToggleSupport :: TaffybarConfig -> IO ()
withToggleSupport config = do
  enabledVar <- MV.newMVar M.empty
  let modified = config { startRefresher = handleToggleRequests enabledVar
                        , getMonitorConfig = toggleableMonitors enabledVar
                        }
  defaultTaffybar modified
