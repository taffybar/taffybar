{-# LANGUAGE OverloadedStrings #-}

-- | Track Chrome tab favicon updates and map them to X11 windows.
module System.Taffybar.Information.Chrome where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import Data.Maybe
import qualified GI.GLib as Gdk
import qualified GI.GdkPixbuf as Gdk
import System.Log.Logger
import System.Taffybar.Context
import System.Taffybar.Information.EWMHDesktopInfo
import System.Taffybar.Information.SafeX11
import Text.Read hiding (lift)
import Text.Regex
import Web.Scotty

-- | Module logger.
logIO :: System.Log.Logger.Priority -> String -> IO ()
logIO = logM "System.Taffybar.Information.Chrome"

-- | Favicon image data associated with a Chrome tab id.
data ChromeTabImageData = ChromeTabImageData
  { tabImageData :: Gdk.Pixbuf,
    tabImageDataId :: Int
  }

-- | Shared favicon table and broadcast update channel.
newtype ChromeTabImageDataState
  = ChromeTabImageDataState
      (MVar (M.Map Int ChromeTabImageData), TChan ChromeTabImageData)

-- | Get or initialize Chrome favicon state.
getChromeTabImageDataState :: TaffyIO ChromeTabImageDataState
getChromeTabImageDataState = do
  ChromeFaviconServerPort port <- fromMaybe (ChromeFaviconServerPort 5000) <$> getState
  getStateDefault (listenForChromeFaviconUpdates port)

-- | Get the broadcast channel for favicon updates.
getChromeTabImageDataChannel :: TaffyIO (TChan ChromeTabImageData)
getChromeTabImageDataChannel = do
  ChromeTabImageDataState (_, chan) <- getChromeTabImageDataState
  return chan

-- | Get the mutable favicon table keyed by tab id.
getChromeTabImageDataTable :: TaffyIO (MVar (M.Map Int ChromeTabImageData))
getChromeTabImageDataTable = do
  ChromeTabImageDataState (table, _) <- getChromeTabImageDataState
  return table

-- | TCP port used by the local Chrome favicon update server.
newtype ChromeFaviconServerPort = ChromeFaviconServerPort Int

-- | Start an HTTP listener that receives favicon updates from browser helpers.
listenForChromeFaviconUpdates :: Int -> TaffyIO ChromeTabImageDataState
listenForChromeFaviconUpdates port = do
  infoVar <- lift $ newMVar M.empty
  inChan <- liftIO newBroadcastTChanIO
  outChan <- liftIO . atomically $ dupTChan inChan
  _ <- lift $
    forkIO $
      scotty port $
        post "/setTabImageData/:tabID" $ do
          tabID <- queryParam "tabID"
          imageData <- LBS.toStrict <$> body
          when (BS.length imageData > 0) $ lift $ do
            loader <- Gdk.pixbufLoaderNew
            Gdk.pixbufLoaderWriteBytes loader =<< Gdk.bytesNew (Just imageData)
            Gdk.pixbufLoaderClose loader
            let updateChannelAndMVar pixbuf =
                  let chromeTabImageData =
                        ChromeTabImageData
                          { tabImageData = pixbuf,
                            tabImageDataId = tabID
                          }
                   in modifyMVar_ infoVar $ \currentMap ->
                        do
                          _ <- atomically $ writeTChan inChan chromeTabImageData
                          return $ M.insert tabID chromeTabImageData currentMap
            Gdk.pixbufLoaderGetPixbuf loader >>= maybe (return ()) updateChannelAndMVar
  return $ ChromeTabImageDataState (infoVar, outChan)

-- | Mapping from X11 window ids to parsed Chrome tab ids.
newtype X11WindowToChromeTabId = X11WindowToChromeTabId (MVar (M.Map X11Window Int))

-- | Get or initialize the X11-window to tab-id mapping.
getX11WindowToChromeTabId :: TaffyIO X11WindowToChromeTabId
getX11WindowToChromeTabId =
  getStateDefault $ X11WindowToChromeTabId <$> maintainX11WindowToChromeTabId

-- | Maintain and update the X11-window to tab-id mapping from title changes.
maintainX11WindowToChromeTabId :: TaffyIO (MVar (M.Map X11Window Int))
maintainX11WindowToChromeTabId = do
  startTabMap <- updateTabMap M.empty
  tabMapVar <- lift $ newMVar startTabMap
  let handleEvent PropertyEvent {ev_window = window} =
        do
          title <- runX11Def "" $ getWindowTitle window
          lift $ modifyMVar_ tabMapVar $ \currentMap -> do
            let newMap = addTabIdEntry currentMap (window, title)
            logIO DEBUG (show newMap)
            return newMap
      handleEvent _ = return ()
  _ <- subscribeToPropertyEvents [ewmhWMName] handleEvent
  return tabMapVar

-- | Regex for tab-id markers embedded in window titles.
tabIDRegex :: Regex
tabIDRegex = mkRegexWithOpts "[|]%([0-9]*)%[|]" True True

-- | Extract a tab id from a window title, if present.
getTabIdFromTitle :: String -> Maybe Int
getTabIdFromTitle title =
  matchRegex tabIDRegex title >>= listToMaybe >>= readMaybe

-- | Insert or update a window->tab mapping based on a title string.
addTabIdEntry :: M.Map X11Window Int -> (X11Window, String) -> M.Map X11Window Int
addTabIdEntry theMap (win, title) =
  maybe theMap ((flip $ M.insert win) theMap) $ getTabIdFromTitle title

-- | Rebuild the window->tab map by scanning all current X11 windows.
updateTabMap :: M.Map X11Window Int -> TaffyIO (M.Map X11Window Int)
updateTabMap tabMap =
  runX11Def tabMap $ do
    wins <- getWindows
    titles <- mapM getWindowTitle wins
    let winsWithTitles = zip wins titles
    return $ foldl addTabIdEntry tabMap winsWithTitles
