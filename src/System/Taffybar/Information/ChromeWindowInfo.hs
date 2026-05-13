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
  _ <- DBus.addMatch client windowUpdatedRule (emitWindowRefresh requestRefresh)
  return ()
  where
    baseRule =
      DBus.matchAny
        { DBus.matchPath = Just "/org/imalison/ChromeWindowInfo",
          DBus.matchInterface = Just "org.imalison.ChromeWindowInfo"
        }
    windowUpdatedRule = baseRule {DBus.matchMember = Just "WindowUpdated"}

emitWindowRefresh :: (WorkspacesRefreshTarget -> IO ()) -> D.Signal -> IO ()
emitWindowRefresh requestRefresh signal = do
  let target =
        case D.signalBody signal of
          windowIdVariant : _
            | Just windowId <- D.fromVariant windowIdVariant ->
                RefreshWindow $ HyprlandWindowIdentity $ normalizeHyprlandWindowId windowId
          _ -> RefreshAllWorkspaces
  requestRefresh target

chromeWindowInfoLog :: Priority -> String -> IO ()
chromeWindowInfoLog =
  logM "System.Taffybar.Information.ChromeWindowInfo"

normalizeHyprlandWindowId :: T.Text -> T.Text
normalizeHyprlandWindowId address =
  let trimmed = T.strip address
   in if "0x" `T.isPrefixOf` trimmed || T.null trimmed
        then trimmed
        else "0x" <> trimmed
