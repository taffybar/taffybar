module System.Taffybar.Hooks
  ( module System.Taffybar.DBus
  , module System.Taffybar.Hooks
  ) where

import Control.Concurrent
import Control.Monad.Trans
import System.Taffybar.Context
import System.Taffybar.DBus
import System.Taffybar.Information.Network
import System.Log.Logger
import Text.Printf

newtype NetworkInfoChan = NetworkInfoChan (Chan [(String, (Rational, Rational))])

buildInfoChan :: RealFrac a1 => a1 -> IO NetworkInfoChan
buildInfoChan interval = do
  chan <- newChan
  let logAndPass v =
        logM "System.Taffybar.Hooks" DEBUG (printf "Got %s" (show v)) >>
             (writeChan chan v)
  _ <- forkIO $ monitorNetworkInterfaces interval logAndPass
  return $ NetworkInfoChan chan

getNetworkChan :: TaffyIO NetworkInfoChan
getNetworkChan = getStateDefault $ lift $ buildInfoChan 2.0
