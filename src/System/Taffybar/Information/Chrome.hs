{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.Information.Chrome where

import           BroadcastChan
import           Control.Monad
import           Control.Monad.Trans.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import           Data.Maybe
import qualified GI.GLib as Gdk
import qualified GI.GdkPixbuf as Gdk
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.Information.EWMHDesktopInfo
import           System.Taffybar.Information.SafeX11 (Event(..))
import           Text.Read hiding (lift)
import           Text.Regex
import           UnliftIO.Concurrent (forkIO)
import qualified UnliftIO.MVar as MV
import           Web.Scotty

logIO :: System.Log.Logger.Priority -> String -> IO ()
logIO = logM "System.Taffybar.Information.Chrome"

data ChromeTabImageData = ChromeTabImageData
  { tabImageData :: Gdk.Pixbuf
  , tabImageDataId :: Int
  }

newtype ChromeTabImageDataState =
  ChromeTabImageDataState
  (MV.MVar (M.Map Int ChromeTabImageData), BroadcastChan Out ChromeTabImageData)

getChromeTabImageDataState :: TaffyIO ChromeTabImageDataState
getChromeTabImageDataState = do
  ChromeFaviconServerPort port <- fromMaybe (ChromeFaviconServerPort 5000) <$> getState
  getStateDefault (listenForChromeFaviconUpdates port)

getChromeTabImageDataChannel :: TaffyIO (BroadcastChan Out ChromeTabImageData)
getChromeTabImageDataChannel = do
  ChromeTabImageDataState (_, chan) <- getChromeTabImageDataState
  return chan

getChromeTabImageDataTable :: TaffyIO (MV.MVar (M.Map Int ChromeTabImageData))
getChromeTabImageDataTable = do
  ChromeTabImageDataState (table, _) <- getChromeTabImageDataState
  return table

newtype ChromeFaviconServerPort = ChromeFaviconServerPort Int

listenForChromeFaviconUpdates :: Int -> TaffyIO ChromeTabImageDataState
listenForChromeFaviconUpdates port = do
  infoVar <- lift $ MV.newMVar M.empty
  inChan <- newBroadcastChan
  outChan <- newBChanListener inChan
  _ <- lift $ forkIO $ scotty port $
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
                    { tabImageData = pixbuf
                    , tabImageDataId = tabID
                    }
              in
                MV.modifyMVar_ infoVar $ \currentMap ->
                  do
                    _ <- writeBChan inChan chromeTabImageData
                    return $ M.insert tabID chromeTabImageData currentMap
        Gdk.pixbufLoaderGetPixbuf loader >>= maybe (return ()) updateChannelAndMVar
  return $ ChromeTabImageDataState (infoVar, outChan)

newtype X11WindowToChromeTabId = X11WindowToChromeTabId (MV.MVar (M.Map X11Window Int))

getX11WindowToChromeTabId :: TaffyIO X11WindowToChromeTabId
getX11WindowToChromeTabId =
  getStateDefault $ X11WindowToChromeTabId <$> maintainX11WindowToChromeTabId

maintainX11WindowToChromeTabId :: TaffyIO (MV.MVar (M.Map X11Window Int))
maintainX11WindowToChromeTabId = do
  startTabMap <- updateTabMap M.empty
  tabMapVar <- lift $ MV.newMVar startTabMap
  let handleEvent PropertyEvent { ev_window = window } =
        do
          title <- runProperty $ getWindowTitle window
          lift $ MV.modifyMVar_ tabMapVar $ \currentMap -> do
            let newMap = addTabIdEntry currentMap (window, title)
            logIO DEBUG (show newMap)
            return newMap
      handleEvent _ = return ()
  _ <- subscribeToPropertyEvents [ewmhWMName] handleEvent
  return tabMapVar

tabIDRegex :: Regex
tabIDRegex = mkRegexWithOpts "[|]%([0-9]*)%[|]" True True

getTabIdFromTitle :: String -> Maybe Int
getTabIdFromTitle title =
  matchRegex tabIDRegex title >>= listToMaybe >>= readMaybe

addTabIdEntry :: M.Map X11Window Int -> (X11Window, String) -> M.Map X11Window Int
addTabIdEntry theMap (win, title) =
          maybe theMap ((flip $ M.insert win) theMap) $ getTabIdFromTitle title

updateTabMap :: M.Map X11Window Int -> TaffyIO (M.Map X11Window Int)
updateTabMap tabMap = runProperty $ do
    wins <- getWindows
    titles <- mapM getWindowTitle wins
    let winsWithTitles = zip wins titles
    return $ foldl addTabIdEntry tabMap winsWithTitles
