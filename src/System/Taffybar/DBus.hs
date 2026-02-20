-- | Convenience combinators for enabling taffybar DBus services.
module System.Taffybar.DBus
  ( module System.Taffybar.DBus.Toggle,
    appendHook,
    startTaffyLogServer,
    withLogServer,
    withToggleServer,
  )
where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import System.Log.DBus.Server
import System.Taffybar.Context
import System.Taffybar.DBus.Toggle

-- | Start the DBus-backed log sink using the shared session client.
startTaffyLogServer :: TaffyIO ()
startTaffyLogServer =
  asks sessionDBusClient >>= lift . startLogServer

-- | Extend a config to start the DBus log server during startup.
withLogServer :: TaffybarConfig -> TaffybarConfig
withLogServer = appendHook startTaffyLogServer

-- | Extend a config with the DBus toggle service.
withToggleServer :: TaffybarConfig -> TaffybarConfig
withToggleServer = handleDBusToggles
