module System.Taffybar.DBus
  ( module System.Taffybar.DBus.Toggle
  , appendHook
  , startTaffyLogServer
  , withLogServer
  , withToggleServer
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import System.Log.DBus.Server
import System.Taffybar.Context
import System.Taffybar.DBus.Toggle

startTaffyLogServer :: TaffyIO ()
startTaffyLogServer =
  asks sessionDBusClient >>= lift . startLogServer

withLogServer :: TaffybarConfig -> TaffybarConfig
withLogServer = appendHook startTaffyLogServer

withToggleServer :: TaffybarConfig -> TaffybarConfig
withToggleServer = handleDBusToggles
