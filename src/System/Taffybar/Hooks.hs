module System.Taffybar.Hooks
  ( module System.Taffybar.DBus
  , module System.Taffybar.Hooks
  , ChromeTabImageData(..)
  , refreshBatteriesOnPropChange
  , getX11WindowToChromeTabId
  , getChromeTabImageDataChannel
  , getChromeTabImageDataTable
  ) where

import           BroadcastChan
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Maybe
import qualified Data.MultiMap as MM
import           System.FilePath
import           System.Taffybar.Context
import           System.Taffybar.DBus
import           System.Taffybar.Information.Battery
import           System.Taffybar.Information.Chrome
import           System.Taffybar.Information.Network
import           System.Taffybar.Information.XDG.DesktopEntry
import           System.Taffybar.Util

newtype NetworkInfoChan = NetworkInfoChan (BroadcastChan In [(String, (Rational, Rational))])

buildInfoChan :: Double -> IO NetworkInfoChan
buildInfoChan interval = do
  chan <- newBroadcastChan
  _ <- forkIO $ monitorNetworkInterfaces interval (void . writeBChan chan)
  return $ NetworkInfoChan chan

getNetworkChan :: TaffyIO NetworkInfoChan
getNetworkChan = getStateDefault $ lift $ buildInfoChan 2.0

withBatteryRefresh :: TaffybarConfig -> TaffybarConfig
withBatteryRefresh = appendHook refreshBatteriesOnPropChange

getDirectoryEntriesByClassName :: TaffyIO (MM.MultiMap String DesktopEntry)
getDirectoryEntriesByClassName =
  getStateDefault readDirectoryEntriesDefault

updateDirectoryEntriesCache :: TaffyIO ()
updateDirectoryEntriesCache = ask >>= \ctx ->
  void $ lift $ foreverWithDelay (60 :: Double) $ flip runReaderT ctx $
       putState readDirectoryEntriesDefault

readDirectoryEntriesDefault :: TaffyIO (MM.MultiMap String DesktopEntry)
readDirectoryEntriesDefault = lift $
  directoryEntriesByClassName <$> getDirectoryEntriesDefault

directoryEntriesByClassName
  :: Foldable t
  => t DesktopEntry -> MM.MultiMap String DesktopEntry
directoryEntriesByClassName = foldl insertByClassName MM.empty
  where
    insertByClassName entriesMap entry =
      MM.insert (getClassName entry) entry entriesMap
    getFromFilename filepath =
      let (_, filename) = splitFileName filepath
          (_, noExtensions) = splitExtensions filename
      in noExtensions
    getClassName DesktopEntry {deAttributes = attributes, deFilename = filename} =
      fromMaybe (getFromFilename filename) $
                lookup "StartupWMClass" attributes <|>
                lookup "Name" attributes
