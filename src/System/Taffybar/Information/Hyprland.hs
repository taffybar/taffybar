{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.Hyprland
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- A small Hyprland "client" that provides a structured interface for issuing
-- Hyprland commands (optionally over the Hyprland command socket) and for
-- connecting to the Hyprland event socket.
--
-- This module is intended to centralize the socket/path logic so widgets can
-- share it. It is not (yet) wired into widgets.
-----------------------------------------------------------------------------

module System.Taffybar.Information.Hyprland
  ( -- * Client
    HyprlandClient
  , HyprlandClientConfig(..)
  , defaultHyprlandClientConfig
  , newHyprlandClient
  , reloadHyprlandClient
  , HyprlandClientEnv(..)
  , getHyprlandClientEnv

    -- * Hyprland Monad
  , HyprlandT
  , runHyprlandT
  , askHyprlandClient
  , runHyprlandCommandRawM
  , runHyprlandCommandJsonM

    -- * Shared Event Channel
  , HyprlandEventChan(..)
  , subscribeHyprlandEvents
  , buildHyprlandEventChan

    -- * Commands
  , HyprlandCommand(..)
  , hyprCommand
  , hyprCommandJson
  , hyprlandCommandToSocketCommand
  , runHyprlandCommandRaw
  , runHyprlandCommandJson

    -- * Sockets
  , HyprlandSocket(..)
  , hyprlandSocketName
  , hyprlandSocketPaths
  , openHyprlandSocket
  , openHyprlandEventSocket
  , withHyprlandEventSocket

    -- * Errors
  , HyprlandError(..)
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM.TChan
  ( TChan
  , dupTChan
  , newBroadcastTChanIO
  , writeTChan
  )
import           Control.Exception.Enclosed (catchAny)
import           Control.Monad (forM, forever, void)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.STM (atomically)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson (FromJSON, eitherDecode')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import           Data.List (sortOn)
import           Data.Ord (Down(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.Environment (lookupEnv)
import           System.FilePath ((</>), takeDirectory, takeFileName)
import           System.IO
  ( BufferMode(LineBuffering)
  , Handle
  , IOMode(ReadWriteMode)
  , hClose
  , hGetLine
  , hSetBuffering
  )
import           System.Log.Logger (Priority(..), logM)
import           System.Posix.Files (getFileStatus, isSocket, modificationTime)
import           System.Posix.Types (EpochTime)
import           Text.Printf (printf)

import           System.Taffybar.Util (runCommand)

data HyprlandError
  = HyprlandEnvMissing String
  | HyprlandSocketUnavailable HyprlandSocket [FilePath]
  | HyprlandSocketException FilePath String
  | HyprlandHyprctlFailed String
  | HyprlandJsonDecodeFailed String
  | HyprlandCommandBuildFailed String
  deriving (Show, Eq)

data HyprlandClientConfig = HyprlandClientConfig
  { useSocket :: Bool
  -- ^ Try the Hyprland command/event sockets when possible.
  , fallbackToHyprctl :: Bool
  -- ^ If the socket path is unavailable, fall back to invoking @hyprctl@.
  , hyprctlPath :: FilePath
  } deriving (Show, Eq)

defaultHyprlandClientConfig :: HyprlandClientConfig
defaultHyprlandClientConfig =
  HyprlandClientConfig
    { useSocket = True
    , fallbackToHyprctl = True
    , hyprctlPath = "hyprctl"
    }

data HyprlandClientEnv = HyprlandClientEnv
  { instanceSignature :: String
  , runtimeDir :: Maybe FilePath
  } deriving (Show, Eq)

getHyprlandClientEnv :: IO (Either HyprlandError HyprlandClientEnv)
getHyprlandClientEnv = do
  mSig <- lookupEnv "HYPRLAND_INSTANCE_SIGNATURE"
  case mSig of
    Nothing ->
      return $ Left $ HyprlandEnvMissing "HYPRLAND_INSTANCE_SIGNATURE"
    Just sig -> do
      mRuntime <- lookupEnv "XDG_RUNTIME_DIR"
      return $ Right $ HyprlandClientEnv
        { instanceSignature = sig
        , runtimeDir = mRuntime
        }

data HyprlandClient = HyprlandClient
  { clientConfig :: HyprlandClientConfig
  , clientEnv :: Maybe HyprlandClientEnv
  } deriving (Show, Eq)

-- | Construct a 'HyprlandClient' by reading environment variables.
--
-- If 'HYPRLAND_INSTANCE_SIGNATURE' is not set, socket-based operations will be
-- unavailable, but command execution may still succeed via the @hyprctl@
-- fallback depending on 'HyprlandClientConfig'.
newHyprlandClient :: HyprlandClientConfig -> IO HyprlandClient
newHyprlandClient cfg = do
  envResult <- getHyprlandClientEnv
  let env = either (const Nothing) Just envResult
  pure $ HyprlandClient { clientConfig = cfg, clientEnv = env }

-- | Reload the environment-derived portions of a 'HyprlandClient'.
reloadHyprlandClient :: HyprlandClient -> IO HyprlandClient
reloadHyprlandClient client = newHyprlandClient (clientConfig client)

-- | A reader transformer that carries a 'HyprlandClient'.
type HyprlandT m a = ReaderT HyprlandClient m a

runHyprlandT :: HyprlandClient -> HyprlandT m a -> m a
runHyprlandT = flip runReaderT

askHyprlandClient :: Monad m => HyprlandT m HyprlandClient
askHyprlandClient = ask

runHyprlandCommandRawM :: (MonadIO m) => HyprlandCommand -> HyprlandT m (Either HyprlandError BS.ByteString)
runHyprlandCommandRawM cmd = do
  client <- ask
  liftIO $ runHyprlandCommandRaw client cmd

runHyprlandCommandJsonM :: (MonadIO m, FromJSON a) => HyprlandCommand -> HyprlandT m (Either HyprlandError a)
runHyprlandCommandJsonM cmd = do
  client <- ask
  liftIO $ runHyprlandCommandJson client cmd

data HyprlandSocket
  = HyprlandCommandSocket
  | HyprlandEventSocket
  deriving (Show, Eq)

hyprlandSocketName :: HyprlandSocket -> FilePath
hyprlandSocketName HyprlandCommandSocket = ".socket.sock"
hyprlandSocketName HyprlandEventSocket = ".socket2.sock"

hyprlandSocketPaths :: HyprlandClient -> HyprlandSocket -> [FilePath]
hyprlandSocketPaths HyprlandClient { clientEnv = Nothing } _ = []
hyprlandSocketPaths
  HyprlandClient
    { clientEnv =
        Just HyprlandClientEnv
          { instanceSignature = sig
          , runtimeDir = mRuntimeDir
          }
    }
  sock =
  let name = hyprlandSocketName sock
      runtimePaths =
        case mRuntimeDir of
          Nothing -> []
          Just rd -> [rd ++ "/hypr/" ++ sig ++ "/" ++ name]
      tmpPath = "/tmp/hypr/" ++ sig ++ "/" ++ name
  in runtimePaths ++ [tmpPath]

openHyprlandSocket :: HyprlandClient -> HyprlandSocket -> IO (Either HyprlandError NS.Socket)
openHyprlandSocket client sock = do
  -- Prefer the instance signature from the process environment first. If that
  -- fails (e.g. Hyprland restarted and the signature changed), fall back to
  -- discovering currently-running instance sockets from the filesystem.
  let envPaths = hyprlandSocketPaths client sock
  envResult <- connectFirst envPaths
  case envResult of
    Just s -> pure (Right s)
    Nothing -> do
      discoveredPaths <- discoverHyprlandSocketPaths client sock
      discResult <- connectFirst discoveredPaths
      case discResult of
        Just s -> pure (Right s)
        Nothing ->
          if null envPaths && null discoveredPaths
            then pure $ Left $ HyprlandEnvMissing "HYPRLAND_INSTANCE_SIGNATURE"
            else pure $ Left $ HyprlandSocketUnavailable sock (envPaths ++ discoveredPaths)
  where
    connectFirst :: [FilePath] -> IO (Maybe NS.Socket)
    connectFirst [] = pure Nothing
    connectFirst (path:rest) = do
      result <- connectToSocket path
      case result of
        Left _ -> connectFirst rest
        Right s -> pure (Just s)

discoverHyprlandSocketPaths :: HyprlandClient -> HyprlandSocket -> IO [FilePath]
discoverHyprlandSocketPaths _client sock = do
  mRuntime <- lookupEnv "XDG_RUNTIME_DIR"
  let bases =
        maybe [] (\rd -> [rd </> "hypr"]) mRuntime ++
        ["/tmp/hypr"]
  let name = hyprlandSocketName sock
  pathsWithTimes <- fmap concat $ forM bases $ \base -> do
    baseExists <- doesDirectoryExist base
    if not baseExists
      then pure []
      else do
        -- Best-effort: Hyprland instances come and go and we don't want to
        -- bring down widgets if this scan races a restart.
        sigEntries <- listDirectory base `catchAny` \_ -> pure []
        let candidates = map (\sig -> base </> sig </> name) sigEntries
        fmap concat $ forM candidates $ \p -> do
          mTime <- socketPathMTime p
          pure $ maybe [] (\t -> [(t, p)]) mTime

  -- Prefer the newest sockets first to minimize binding to a stale instance.
  pure $ map snd $ sortOn (Down . fst) pathsWithTimes

socketPathMTime :: FilePath -> IO (Maybe EpochTime)
socketPathMTime path =
  (do
      st <- getFileStatus path
      if isSocket st
        then pure $ Just $ modificationTime st
        else pure Nothing
    ) `catchAny` \_ -> pure Nothing

connectToSocket :: FilePath -> IO (Either HyprlandError NS.Socket)
connectToSocket path = do
  sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
  (do
      NS.connect sock (NS.SockAddrUnix path)
      return $ Right sock
    ) `catchAny` \e -> do
      void $ NS.close sock `catchAny` \_ -> pure ()
      return $ Left $ HyprlandSocketException path (show e)

openHyprlandEventSocket :: HyprlandClient -> IO (Either HyprlandError Handle)
openHyprlandEventSocket client = do
  sockResult <- openHyprlandSocket client HyprlandEventSocket
  case sockResult of
    Left err -> return (Left err)
    Right sock -> do
      handle <- NS.socketToHandle sock ReadWriteMode
      hSetBuffering handle LineBuffering
      return (Right handle)

withHyprlandEventSocket :: HyprlandClient -> (Handle -> IO a) -> IO (Either HyprlandError a)
withHyprlandEventSocket client action = do
  handleResult <- openHyprlandEventSocket client
  case handleResult of
    Left err -> return (Left err)
    Right handle ->
      (do
          result <- action handle
          hClose handle
          return (Right result)
        ) `catchAny` \e -> do
          hClose handle
          return (Left $ HyprlandSocketException (show HyprlandEventSocket) (show e))

-- | A shared broadcast channel for Hyprland events read from the event socket.
--
-- Readers should call 'subscribeHyprlandEvents' to get their own cursor.
newtype HyprlandEventChan =
  HyprlandEventChan (TChan T.Text)

subscribeHyprlandEvents :: HyprlandEventChan -> IO (TChan T.Text)
subscribeHyprlandEvents (HyprlandEventChan chan) =
  atomically $ dupTChan chan

buildHyprlandEventChan :: HyprlandClient -> IO HyprlandEventChan
buildHyprlandEventChan client = do
  chan <- newBroadcastTChanIO
  _ <- forkIO $ eventThread chan
  pure $ HyprlandEventChan chan
  where
    logH :: Priority -> String -> IO ()
    logH = logM "System.Taffybar.Information.Hyprland"

    retryDelayMicros :: Int
    retryDelayMicros = 1 * 1000000

    eventThread chan = forever $ do
      handleResult <- openHyprlandEventSocket client
      case handleResult of
        Left err -> do
          logH WARNING $ printf "Hyprland event socket unavailable: %s" (show err)
          threadDelay retryDelayMicros
        Right handle -> do
          -- Emit a synthetic event on (re)connect so widgets can refresh their
          -- state after Hyprland restarts without resorting to polling.
          atomically $ writeTChan chan "taffybar-hyprland-connected>>"
          let loop =
                hGetLine handle >>= (atomically . writeTChan chan . T.pack) >> loop
          loop `catchAny` \e ->
            logH WARNING $ printf "Hyprland event socket failed: %s" (show e)
          void $ hClose handle `catchAny` \_ -> pure ()
          threadDelay retryDelayMicros

data HyprlandCommand = HyprlandCommand
  { commandArgs :: [String]
  , commandJson :: Bool
  } deriving (Show, Eq)

hyprCommand :: [String] -> HyprlandCommand
hyprCommand args = HyprlandCommand { commandArgs = args, commandJson = False }

hyprCommandJson :: [String] -> HyprlandCommand
hyprCommandJson args = HyprlandCommand { commandArgs = args, commandJson = True }

hyprlandCommandToSocketCommand :: HyprlandCommand -> Either HyprlandError BS.ByteString
hyprlandCommandToSocketCommand HyprlandCommand { commandArgs = args, commandJson = isJson }
  | null args = Left $ HyprlandCommandBuildFailed "No Hyprland command provided"
  | isJson = Right $ BS8.pack $ "j/" ++ unwords args
  | otherwise = Right $ BS8.pack $ unwords args

-- | Run a Hyprland command, preferring the command socket if enabled.
--
-- If socket execution fails and 'fallbackToHyprctl' is enabled, @hyprctl@ will
-- be invoked as a fallback.
runHyprlandCommandRaw :: HyprlandClient -> HyprlandCommand -> IO (Either HyprlandError BS.ByteString)
runHyprlandCommandRaw
  client@HyprlandClient
    { clientConfig =
        HyprlandClientConfig
          { useSocket = useSock
          , fallbackToHyprctl = fallback
          , hyprctlPath = hyprctl
          }
    }
  cmd = do
  socketResult <-
    if useSock
      then runHyprlandCommandSocket client cmd
      else pure $ Left $ HyprlandSocketUnavailable HyprlandCommandSocket []
  case socketResult of
    Right out -> pure (Right out)
    Left sockErr ->
      if fallback
        then do
          hyprctlResult <- runHyprlandCommandHyprctl client hyprctl cmd
          pure $ case hyprctlResult of
            Right out -> Right out
            Left hyprctlErr -> Left $ HyprlandHyprctlFailed $
              printf "%s (socket error: %s)" hyprctlErr (show sockErr)
        else pure (Left sockErr)

runHyprlandCommandSocket :: HyprlandClient -> HyprlandCommand -> IO (Either HyprlandError BS.ByteString)
runHyprlandCommandSocket client cmd = do
  sockResult <- openHyprlandSocket client HyprlandCommandSocket
  case sockResult of
    Left err -> pure (Left err)
    Right sock ->
      (do
          cmdBytes <- case hyprlandCommandToSocketCommand cmd of
            Left e -> NS.close sock >> pure (Left e)
            Right b -> pure (Right b)
          case cmdBytes of
            Left e -> pure (Left e)
            Right b -> do
              NSB.sendAll sock b
              NS.shutdown sock NS.ShutdownSend
              resp <- recvAll sock
              NS.close sock
              pure (Right resp)
        ) `catchAny` \e -> do
          NS.close sock
          pure $ Left $ HyprlandSocketException (show HyprlandCommandSocket) (show e)

runHyprlandCommandHyprctl :: HyprlandClient -> FilePath -> HyprlandCommand -> IO (Either String BS.ByteString)
runHyprlandCommandHyprctl client hyprctl HyprlandCommand { commandArgs = args, commandJson = isJson } = do
  mSig <- pickHyprlandInstanceSignature client
  let flags =
        ["-j" | isJson] ++
        maybe [] (\sig -> ["-i", sig]) mSig
  result <- runCommand hyprctl (flags ++ args)
  pure $ case result of
    Left err -> Left err
    Right out -> Right $ TE.encodeUtf8 $ T.pack out

pickHyprlandInstanceSignature :: HyprlandClient -> IO (Maybe String)
pickHyprlandInstanceSignature client =
  case clientEnv client of
    Just HyprlandClientEnv { instanceSignature = sig } -> do
      -- Prefer the signature from the environment if we can actually connect
      -- to the command socket. (After a Hyprland restart the old socket path
      -- might still exist but be stale.)
      envAlive <- anyM canConnectSocket $
        hyprlandSocketPaths client HyprlandCommandSocket
      if envAlive
        then pure (Just sig)
        else discover
    Nothing -> discover
  where
    canConnectSocket path = do
      result <- connectToSocket path
      case result of
        Left _ -> pure False
        Right sock -> do
          void $ NS.close sock `catchAny` \_ -> pure ()
          pure True

    discover = do
      -- Use the newest discovered command socket.
      paths <- discoverHyprlandSocketPaths client HyprlandCommandSocket
      pure $ case paths of
        p:_ -> Just $ takeFileName $ takeDirectory p
        [] -> Nothing

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM p (x:xs) = do
  b <- p x
  if b then pure True else anyM p xs

recvAll :: NS.Socket -> IO BS.ByteString
recvAll sock = go []
  where
    go acc = do
      chunk <- NSB.recv sock 4096
      if BS.null chunk
        then return (BS.concat (reverse acc))
        else go (chunk:acc)

runHyprlandCommandJson :: FromJSON a => HyprlandClient -> HyprlandCommand -> IO (Either HyprlandError a)
runHyprlandCommandJson client cmd = do
  raw <- runHyprlandCommandRaw client cmd
  pure $ case raw of
    Left err -> Left err
    Right out ->
      case eitherDecode' (BL.fromStrict out) of
        Left decodeErr -> Left $ HyprlandJsonDecodeFailed decodeErr
        Right a -> Right a
