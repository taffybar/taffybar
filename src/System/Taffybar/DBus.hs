module System.Taffybar.DBus
  ( module System.Taffybar.DBus.Toggle,
    appendHook,
    startTaffyLogServer,
    withLogServer,
    withToggleServer,
  )
where

import System.Log.DBus.Server
import System.Taffybar.Context
import System.Taffybar.DBus.Toggle

startTaffyLogServer :: TaffyIO ()
startTaffyLogServer = installSessionDBusClientHook startLogServer

withLogServer :: TaffybarConfig -> TaffybarConfig
withLogServer = appendHook startTaffyLogServer

withToggleServer :: TaffybarConfig -> TaffybarConfig
withToggleServer = handleDBusToggles
