{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.Workspaces.RiverXMonad
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Shared workspace-state provider for modern river sessions managed by
-- xmonad-river. xmonad-river writes a compact status file under
-- @$XDG_STATE_HOME/river-xmonad/status.tsv@; this provider polls that file and
-- exposes it through the same backend-agnostic model as the Hyprland provider.
module System.Taffybar.Information.Workspaces.RiverXMonad
  ( RiverXMonadWorkspaceProviderConfig (..),
    defaultRiverXMonadWorkspaceProviderConfig,
    defaultRiverXMonadWorkspaceState,
    isRiverXMonadSession,
    riverXMonadStatusPath,
    getRiverXMonadWorkspaceStateChanAndVar,
    getRiverXMonadWorkspaceStateChanAndVarWith,
    getRiverXMonadWorkspaceState,
    getRiverXMonadWorkspaceStateWith,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception (IOException, catch)
import Control.Exception.Enclosed (catchAny)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.STM (atomically)
import Data.Char (toLower)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Log.Logger (Priority (..), logM)
import System.Taffybar.Context (TaffyIO, getStateDefault, taffyFork)
import System.Taffybar.Information.Workspaces.Model

data RiverXMonadWorkspaceProviderConfig = RiverXMonadWorkspaceProviderConfig
  { riverXMonadSnapshotGetter :: TaffyIO (Bool, [WorkspaceInfo]),
    riverXMonadPollIntervalMicroseconds :: Int
  }

defaultRiverXMonadWorkspaceProviderConfig :: RiverXMonadWorkspaceProviderConfig
defaultRiverXMonadWorkspaceProviderConfig =
  RiverXMonadWorkspaceProviderConfig
    { riverXMonadSnapshotGetter = buildRiverXMonadWorkspaceSnapshot,
      riverXMonadPollIntervalMicroseconds = 250000
    }

defaultRiverXMonadWorkspaceState :: WorkspaceSnapshot
defaultRiverXMonadWorkspaceState =
  WorkspaceSnapshot
    { snapshotBackend = WorkspaceBackendRiverXMonad,
      snapshotRevision = 0,
      snapshotWindowDataComplete = True,
      snapshotWorkspaces = []
    }

newtype RiverXMonadWorkspaceStateChanVar
  = RiverXMonadWorkspaceStateChanVar (TChan WorkspaceSnapshot, MVar WorkspaceSnapshot)

wLog :: (MonadIO m) => Priority -> String -> m ()
wLog level msg = liftIO $ logM "System.Taffybar.Information.Workspaces.RiverXMonad" level msg

isRiverXMonadSession :: IO Bool
isRiverXMonadSession = do
  wm <- fmap normalize <$> lookupEnv "IMALISON_WINDOW_MANAGER"
  session <- fmap normalize <$> lookupEnv "XDG_SESSION_DESKTOP"
  desktop <- fmap normalize <$> lookupEnv "XDG_CURRENT_DESKTOP"
  pure $
    wm == Just "river-xmonad"
      || session == Just "river-xmonad"
      || desktop == Just "river"
  where
    normalize = map toLower

getRiverXMonadWorkspaceStateChanAndVar ::
  TaffyIO (TChan WorkspaceSnapshot, MVar WorkspaceSnapshot)
getRiverXMonadWorkspaceStateChanAndVar =
  getRiverXMonadWorkspaceStateChanAndVarWith defaultRiverXMonadWorkspaceProviderConfig

getRiverXMonadWorkspaceStateChanAndVarWith ::
  RiverXMonadWorkspaceProviderConfig ->
  TaffyIO (TChan WorkspaceSnapshot, MVar WorkspaceSnapshot)
getRiverXMonadWorkspaceStateChanAndVarWith cfg = do
  RiverXMonadWorkspaceStateChanVar chanAndVar <-
    getStateDefault $ buildRiverXMonadWorkspaceStateChanVar cfg
  pure chanAndVar

getRiverXMonadWorkspaceState :: TaffyIO WorkspaceSnapshot
getRiverXMonadWorkspaceState =
  getRiverXMonadWorkspaceStateWith defaultRiverXMonadWorkspaceProviderConfig

getRiverXMonadWorkspaceStateWith ::
  RiverXMonadWorkspaceProviderConfig ->
  TaffyIO WorkspaceSnapshot
getRiverXMonadWorkspaceStateWith cfg = do
  (_, stateVar) <- getRiverXMonadWorkspaceStateChanAndVarWith cfg
  liftIO $ readMVar stateVar

buildRiverXMonadWorkspaceStateChanVar ::
  RiverXMonadWorkspaceProviderConfig ->
  TaffyIO RiverXMonadWorkspaceStateChanVar
buildRiverXMonadWorkspaceStateChanVar cfg = do
  stateChan <- liftIO newBroadcastTChanIO
  stateVar <- liftIO $ newMVar defaultRiverXMonadWorkspaceState
  taffyFork $ riverXMonadWorkspaceStateLoop cfg stateChan stateVar
  pure $ RiverXMonadWorkspaceStateChanVar (stateChan, stateVar)

riverXMonadWorkspaceStateLoop ::
  RiverXMonadWorkspaceProviderConfig ->
  TChan WorkspaceSnapshot ->
  MVar WorkspaceSnapshot ->
  TaffyIO ()
riverXMonadWorkspaceStateLoop cfg stateChan stateVar = forever $ do
  refreshRiverXMonadWorkspaceState cfg stateChan stateVar
  liftIO $ threadDelay (riverXMonadPollIntervalMicroseconds cfg)

refreshRiverXMonadWorkspaceState ::
  RiverXMonadWorkspaceProviderConfig ->
  TChan WorkspaceSnapshot ->
  MVar WorkspaceSnapshot ->
  TaffyIO ()
refreshRiverXMonadWorkspaceState cfg stateChan stateVar = do
  previous <- liftIO $ readMVar stateVar
  (complete, workspaces) <-
    riverXMonadSnapshotGetter cfg
      `catchAny` \err -> do
        wLog WARNING $ "river-xmonad workspace snapshot update failed: " <> show err
        pure (False, snapshotWorkspaces previous)
  let changed =
        snapshotWindowDataComplete previous /= complete
          || snapshotWorkspaces previous /= workspaces
      next =
        WorkspaceSnapshot
          { snapshotBackend = WorkspaceBackendRiverXMonad,
            snapshotRevision = snapshotRevision previous + 1,
            snapshotWindowDataComplete = complete,
            snapshotWorkspaces = workspaces
          }
  when changed $
    liftIO $ do
      _ <- swapMVar stateVar next
      atomically $ writeTChan stateChan next

buildRiverXMonadWorkspaceSnapshot :: TaffyIO (Bool, [WorkspaceInfo])
buildRiverXMonadWorkspaceSnapshot = liftIO $ do
  path <- riverXMonadStatusPath
  exists <- doesFileExist path
  if not exists
    then pure (False, [])
    else do
      content <- readFileIfExists path
      pure (True, parseRiverXMonadStatus content)

riverXMonadStatusPath :: IO FilePath
riverXMonadStatusPath = do
  override <- lookupEnv "TAFFYBAR_RIVER_XMONAD_STATUS"
  case override of
    Just path -> pure path
    Nothing -> do
      stateHome <- lookupEnv "XDG_STATE_HOME"
      home <- lookupEnv "HOME"
      let base =
            case (stateHome, home) of
              (Just value, _) -> value
              (Nothing, Just value) -> value </> ".local" </> "state"
              (Nothing, Nothing) -> "."
      pure $ base </> "river-xmonad" </> "status.tsv"

data RawWorkspace = RawWorkspace
  { rawWorkspaceName :: T.Text,
    rawWorkspaceState :: WorkspaceViewState,
    rawWorkspaceSpecial :: Bool
  }

data RawWindow = RawWindow
  { rawWindowWorkspace :: T.Text,
    rawWindowId :: T.Text,
    rawWindowTitle :: T.Text,
    rawWindowAppId :: T.Text,
    rawWindowActive :: Bool,
    rawWindowMinimized :: Bool,
    rawWindowPosition :: Maybe (Int, Int)
  }

parseRiverXMonadStatus :: String -> [WorkspaceInfo]
parseRiverXMonadStatus content =
  map toWorkspace sortedWorkspaces
  where
    parsed = mapMaybe parseLine (T.lines $ T.pack content)
    workspaces =
      [ws | Left ws <- parsed]
    windows =
      [win | Right win <- parsed]
    sortedWorkspaces =
      sortOn (workspaceSortKey . rawWorkspaceName) workspaces
    windowsByWorkspace =
      M.fromListWith (++) [(rawWindowWorkspace win, [toWindow win]) | win <- windows]
    toWorkspace RawWorkspace {rawWorkspaceName = name, rawWorkspaceState = state, rawWorkspaceSpecial = special} =
      WorkspaceInfo
        { workspaceIdentity =
            WorkspaceIdentity
              { workspaceNumericId = parseInt name,
                workspaceName = name
              },
          workspaceState = state,
          workspaceHasUrgentWindow = False,
          workspaceIsSpecial = special,
          workspaceWindows = reverse $ M.findWithDefault [] name windowsByWorkspace
        }

toWindow :: RawWindow -> WindowInfo
toWindow RawWindow {rawWindowId = winId, rawWindowTitle = title, rawWindowAppId = appId, rawWindowActive = active, rawWindowMinimized = minimized, rawWindowPosition = pos} =
  WindowInfo
    { windowIdentity = RiverXMonadWindowIdentity winId,
      windowTitle = title,
      windowClassHints = [appId | not $ T.null appId],
      windowPosition = pos,
      windowUrgent = False,
      windowActive = active,
      windowMinimized = minimized
    }

parseLine :: T.Text -> Maybe (Either RawWorkspace RawWindow)
parseLine lineText =
  case T.splitOn "\t" lineText of
    ["workspace", name, state, special] ->
      Just $
        Left $
          RawWorkspace
            { rawWorkspaceName = name,
              rawWorkspaceState = parseWorkspaceState state,
              rawWorkspaceSpecial = parseBool special
            }
    ["window", workspace, winId, title, appId, active, minimized, x, y] ->
      Just $
        Right $
          RawWindow
            { rawWindowWorkspace = workspace,
              rawWindowId = winId,
              rawWindowTitle = title,
              rawWindowAppId = appId,
              rawWindowActive = parseBool active,
              rawWindowMinimized = parseBool minimized,
              rawWindowPosition = (,) <$> parseInt x <*> parseInt y
            }
    _ -> Nothing

parseWorkspaceState :: T.Text -> WorkspaceViewState
parseWorkspaceState "active" = WorkspaceActive
parseWorkspaceState "visible" = WorkspaceVisible
parseWorkspaceState "empty" = WorkspaceEmpty
parseWorkspaceState _ = WorkspaceHidden

parseBool :: T.Text -> Bool
parseBool "true" = True
parseBool "1" = True
parseBool _ = False

parseInt :: T.Text -> Maybe Int
parseInt text =
  case TR.signed TR.decimal text of
    Right (value, rest) | T.null rest -> Just value
    _ -> Nothing

workspaceSortKey :: T.Text -> (Int, T.Text)
workspaceSortKey name =
  case parseInt name of
    Just number -> (number, name)
    Nothing -> (maxBound, name)

readFileIfExists :: FilePath -> IO String
readFileIfExists path =
  readFile path `catch` \(_ :: IOException) -> pure ""
