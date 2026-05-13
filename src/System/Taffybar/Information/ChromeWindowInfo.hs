{-# LANGUAGE OverloadedStrings #-}

-- | Event source for the chrome-favicon-bridge D-Bus service.
module System.Taffybar.Information.ChromeWindowInfo
  ( registerChromeWindowInfoRefreshRequests,
  )
where

import Control.Exception.Enclosed (catchAny)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, asks, runReaderT)
import qualified DBus as D
import qualified DBus.Client as DBus
import qualified Data.Text as T
import System.Log.Logger (Priority (..), logM)
import System.Taffybar.Context (TaffyIO, getStateDefault, sessionDBusClient)
import qualified System.Taffybar.DBus.Client.ChromeWindowInfo as ChromeWindowInfoDBus
import System.Taffybar.Information.Workspaces.Model
  ( WindowIdentity (..),
    WorkspacesRefreshTarget (..),
  )
import System.Taffybar.Information.Workspaces.Refresh (requestWorkspacesRefresh)

data ChromeWindowInfoRefreshSubscription
  = ChromeWindowInfoRefreshSubscription

registerChromeWindowInfoRefreshRequests :: TaffyIO ()
registerChromeWindowInfoRefreshRequests = do
  _ <-
    getStateDefault $
      buildChromeWindowInfoRefreshSubscription
        >> return ChromeWindowInfoRefreshSubscription
  return ()

buildChromeWindowInfoRefreshSubscription :: TaffyIO ()
buildChromeWindowInfoRefreshSubscription = do
  ctx <- ask
  client <- asks sessionDBusClient
  let requestRefresh target =
        runReaderT (requestWorkspacesRefresh target) ctx
  liftIO $
    ( do
        registerChromeWindowInfoSignals client requestRefresh
        logM
          "System.Taffybar.Information.ChromeWindowInfo"
          DEBUG
          "Subscribed to Chrome window info signals"
    )
      `catchAny` \err ->
        chromeWindowInfoLog
          WARNING
          ("Failed to subscribe to Chrome window info signals: " <> show err)

registerChromeWindowInfoSignals ::
  DBus.Client ->
  (WorkspacesRefreshTarget -> IO ()) ->
  IO ()
registerChromeWindowInfoSignals client requestRefresh = do
  _ <-
    ChromeWindowInfoDBus.registerForWindowUpdated
      client
      DBus.matchAny
      (emitWindowRefresh requestRefresh)
      logMalformedWindowUpdatedSignal
  return ()

emitWindowRefresh :: (WorkspacesRefreshTarget -> IO ()) -> D.Signal -> String -> String -> IO ()
emitWindowRefresh requestRefresh _signal windowId _payload =
  requestRefresh $
    RefreshWindow $
      HyprlandWindowIdentity $
        normalizeHyprlandWindowId $
          T.pack windowId

logMalformedWindowUpdatedSignal :: D.Signal -> IO ()
logMalformedWindowUpdatedSignal signal =
  chromeWindowInfoLog
    WARNING
    ("Ignoring malformed ChromeWindowInfo WindowUpdated signal: " <> show (D.signalBody signal))

chromeWindowInfoLog :: Priority -> String -> IO ()
chromeWindowInfoLog =
  logM "System.Taffybar.Information.ChromeWindowInfo"

normalizeHyprlandWindowId :: T.Text -> T.Text
normalizeHyprlandWindowId address =
  let trimmed = T.strip address
   in if "0x" `T.isPrefixOf` trimmed || T.null trimmed
        then trimmed
        else "0x" <> trimmed
