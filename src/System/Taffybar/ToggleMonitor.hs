{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.ToggleMonitor (
  handleToggleRequests,
  toggleableMonitors,
  withToggleServer
) where

import           Control.Concurrent
import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.Map as M
import           Data.Maybe
import qualified Graphics.UI.Gtk as Gtk
import           Graphics.UI.Gtk.Gdk.Screen
import           System.Taffybar
import           Text.Read hiding (get, lift)
import           Web.Scotty
import           XMonad.Core ( whenJust )

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

handleToggleRequests :: Int -> MV.MVar (M.Map Int Bool) -> IO () -> IO ()
handleToggleRequests port enabledVar refreshTaffyWindows = do
  let toggleTaffyOnMon fn mon = do
        MV.modifyMVar_ enabledVar $ \numToEnabled -> do
          let current = fromMaybe True $ M.lookup mon numToEnabled
          return $ M.insert mon (fn current) numToEnabled
        refreshTaffyWindows
      toggleTaffy = do
        num <- liftIO $ runMaybeT getActiveScreenNumber
        liftIO $ toggleTaffyOnMon not $ fromMaybe 0 num
      runScotty =
        scotty port $ do
          get "/toggle/:monNum" $ do
            num <- param "monNum"
            liftIO $
              whenJust (readMaybe num :: Maybe Int) $ toggleTaffyOnMon not
          get "/on/:monNum" $ do
            num <- param "monNum"
            liftIO $
              whenJust (readMaybe num :: Maybe Int) $
              toggleTaffyOnMon $ const True
          get "/off/:monNum" $ do
            num <- param "monNum"
            liftIO $
              whenJust (readMaybe num :: Maybe Int) $
              toggleTaffyOnMon $ const False
          get "/toggleCurrent" $ liftIO $ Gtk.postGUIAsync toggleTaffy
  void $ forkIO runScotty

withToggleServer :: Int -> TaffybarConfig -> IO ()
withToggleServer port config = do
  enabledVar <- MV.newMVar M.empty
  let modified = config { startRefresher = handleToggleRequests port enabledVar
                        , getMonitorConfig = toggleableMonitors enabledVar
                        }
  defaultTaffybar modified
