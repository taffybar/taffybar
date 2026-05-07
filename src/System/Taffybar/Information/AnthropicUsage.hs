{-# LANGUAGE OverloadedStrings #-}

-- | Shared Claude Code subscription usage information.
--
-- Claude Code currently stores local transcript JSONL files under
-- @~/.claude/projects@. This module derives usage from those local transcripts
-- and reads subscription metadata from Claude Code's local state files. It does
-- not call an Anthropic billing endpoint.
module System.Taffybar.Information.AnthropicUsage
  ( AnthropicUsageConfig (..),
    defaultAnthropicUsageConfig,
    AnthropicUsageSnapshot (..),
    AnthropicUsageInfo (..),
    AnthropicUsageWindow (..),
    AnthropicUsageTotals (..),
    getAnthropicUsageInfo,
    updateAnthropicUsage,
    getAnthropicUsageChan,
    getAnthropicUsageState,
    forceAnthropicUsageRefresh,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically, orElse)
import Control.Concurrent.STM.TChan
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getHomeDirectory,
    getModificationTime,
    listDirectory,
  )
import System.FilePath (takeExtension, (</>))
import System.Log.Logger (Priority (WARNING), logM)
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Information.Wakeup (getWakeupChannelForDelay)

data AnthropicUsageConfig = AnthropicUsageConfig
  { anthropicUsagePollInterval :: Double,
    anthropicUsageStatePath :: Maybe FilePath,
    anthropicUsageCredentialsPath :: Maybe FilePath,
    anthropicUsageProjectsPath :: Maybe FilePath,
    anthropicUsageFileLookbackSeconds :: NominalDiffTime,
    anthropicUsageFiveHourBudgetTokens :: Maybe Int,
    anthropicUsageWeeklyBudgetTokens :: Maybe Int
  }

defaultAnthropicUsageConfig :: AnthropicUsageConfig
defaultAnthropicUsageConfig =
  AnthropicUsageConfig
    { anthropicUsagePollInterval = 60 * 5,
      anthropicUsageStatePath = Nothing,
      anthropicUsageCredentialsPath = Nothing,
      anthropicUsageProjectsPath = Nothing,
      anthropicUsageFileLookbackSeconds = 8 * 24 * 60 * 60,
      anthropicUsageFiveHourBudgetTokens = Nothing,
      anthropicUsageWeeklyBudgetTokens = Nothing
    }

data AnthropicUsageSnapshot
  = AnthropicUsageAvailable AnthropicUsageInfo
  | AnthropicUsageUnavailable T.Text
  deriving (Eq, Show)

data AnthropicUsageInfo = AnthropicUsageInfo
  { anthropicUsageGeneratedAt :: UTCTime,
    anthropicUsageSubscriptionType :: Maybe T.Text,
    anthropicUsageRateLimitTier :: Maybe T.Text,
    anthropicUsageHasAvailableSubscription :: Maybe Bool,
    anthropicUsageExtraUsageDisabledReason :: Maybe T.Text,
    anthropicUsageOrganizationName :: Maybe T.Text,
    anthropicUsageFiveHourWindow :: AnthropicUsageWindow,
    anthropicUsageWeeklyWindow :: AnthropicUsageWindow
  }
  deriving (Eq, Show)

data AnthropicUsageWindow = AnthropicUsageWindow
  { anthropicUsageWindowName :: T.Text,
    anthropicUsageWindowStart :: UTCTime,
    anthropicUsageWindowEnd :: UTCTime,
    anthropicUsageWindowBudgetTokens :: Maybe Int,
    anthropicUsageWindowTotals :: AnthropicUsageTotals
  }
  deriving (Eq, Show)

data AnthropicUsageTotals = AnthropicUsageTotals
  { anthropicUsageRequestCount :: Int,
    anthropicUsageInputTokens :: Int,
    anthropicUsageCacheCreationTokens :: Int,
    anthropicUsageCacheReadTokens :: Int,
    anthropicUsageOutputTokens :: Int,
    anthropicUsageEstimatedCostUSD :: Maybe Double
  }
  deriving (Eq, Show)

instance Semigroup AnthropicUsageTotals where
  a <> b =
    AnthropicUsageTotals
      { anthropicUsageRequestCount = anthropicUsageRequestCount a + anthropicUsageRequestCount b,
        anthropicUsageInputTokens = anthropicUsageInputTokens a + anthropicUsageInputTokens b,
        anthropicUsageCacheCreationTokens = anthropicUsageCacheCreationTokens a + anthropicUsageCacheCreationTokens b,
        anthropicUsageCacheReadTokens = anthropicUsageCacheReadTokens a + anthropicUsageCacheReadTokens b,
        anthropicUsageOutputTokens = anthropicUsageOutputTokens a + anthropicUsageOutputTokens b,
        anthropicUsageEstimatedCostUSD =
          addMaybeDouble
            (anthropicUsageEstimatedCostUSD a)
            (anthropicUsageEstimatedCostUSD b)
      }

instance Monoid AnthropicUsageTotals where
  mempty =
    AnthropicUsageTotals
      { anthropicUsageRequestCount = 0,
        anthropicUsageInputTokens = 0,
        anthropicUsageCacheCreationTokens = 0,
        anthropicUsageCacheReadTokens = 0,
        anthropicUsageOutputTokens = 0,
        anthropicUsageEstimatedCostUSD = Nothing
      }

addMaybeDouble :: Maybe Double -> Maybe Double -> Maybe Double
addMaybeDouble Nothing Nothing = Nothing
addMaybeDouble a b = Just $ fromMaybe 0 a + fromMaybe 0 b

data AnthropicUsageMetadata = AnthropicUsageMetadata
  { anthropicMetadataSubscriptionType :: Maybe T.Text,
    anthropicMetadataRateLimitTier :: Maybe T.Text,
    anthropicMetadataHasAvailableSubscription :: Maybe Bool,
    anthropicMetadataExtraUsageDisabledReason :: Maybe T.Text,
    anthropicMetadataOrganizationName :: Maybe T.Text
  }
  deriving (Eq, Show)

data AnthropicCredentials = AnthropicCredentials
  { anthropicCredentialsSubscriptionType :: Maybe T.Text,
    anthropicCredentialsRateLimitTier :: Maybe T.Text
  }

instance FromJSON AnthropicCredentials where
  parseJSON = withObject "AnthropicCredentials" $ \root -> do
    oauth <- root .:? "claudeAiOauth"
    case oauth of
      Nothing -> return $ AnthropicCredentials Nothing Nothing
      Just value ->
        withObject
          "ClaudeAiOauth"
          ( \o ->
              AnthropicCredentials
                <$> o .:? "subscriptionType"
                <*> o .:? "rateLimitTier"
          )
          value

data AnthropicState = AnthropicState
  { anthropicStateHasAvailableSubscription :: Maybe Bool,
    anthropicStateExtraUsageDisabledReason :: Maybe T.Text,
    anthropicStateOrganizationName :: Maybe T.Text
  }

instance FromJSON AnthropicState where
  parseJSON = withObject "AnthropicState" $ \o -> do
    oauth <- o .:? "oauthAccount"
    organizationName <-
      maybe
        (return Nothing)
        (withObject "OauthAccount" (.:? "organizationName"))
        oauth
    AnthropicState
      <$> o .:? "hasAvailableSubscription"
      <*> o .:? "cachedExtraUsageDisabledReason"
      <*> pure organizationName

data TranscriptUsageEntry = TranscriptUsageEntry
  { transcriptUsageTimestamp :: UTCTime,
    transcriptUsageRequestId :: T.Text,
    transcriptUsageTotals :: AnthropicUsageTotals
  }
  deriving (Eq, Show)

data TranscriptJSON = TranscriptJSON
  { transcriptJSONTimestamp :: Maybe UTCTime,
    transcriptJSONRequestId :: Maybe T.Text,
    transcriptJSONUsage :: Maybe AnthropicUsageTotals,
    transcriptJSONCost :: Maybe Double
  }

instance FromJSON TranscriptJSON where
  parseJSON = withObject "TranscriptJSON" $ \o -> do
    message <- o .:? "message"
    usage <- parseMessageUsage message
    TranscriptJSON
      <$> o .:? "timestamp"
      <*> o .:? "requestId"
      <*> pure usage
      <*> o .:? "costUSD"

parseMessageUsage :: Maybe Value -> Parser (Maybe AnthropicUsageTotals)
parseMessageUsage Nothing = return Nothing
parseMessageUsage (Just value) =
  withObject
    "Message"
    ( \message ->
        message .:? "usage" >>= traverse parseJSON
    )
    value

instance FromJSON AnthropicUsageTotals where
  parseJSON = withObject "AnthropicUsageTotals" $ \o -> do
    input <- o .:? "input_tokens" .!= 0
    cacheCreation <- o .:? "cache_creation_input_tokens" .!= 0
    cacheRead <- o .:? "cache_read_input_tokens" .!= 0
    output <- o .:? "output_tokens" .!= 0
    return $
      AnthropicUsageTotals
        { anthropicUsageRequestCount = 1,
          anthropicUsageInputTokens = input,
          anthropicUsageCacheCreationTokens = cacheCreation,
          anthropicUsageCacheReadTokens = cacheRead,
          anthropicUsageOutputTokens = output,
          anthropicUsageEstimatedCostUSD = Nothing
        }

getAnthropicUsageInfo :: AnthropicUsageConfig -> IO AnthropicUsageInfo
getAnthropicUsageInfo config = do
  now <- getCurrentTime
  metadata <- readAnthropicUsageMetadata config
  entries <- readAnthropicTranscriptEntries config now
  let fiveHourWindow =
        buildActiveBlockWindow
          now
          "5h"
          (5 * 60 * 60)
          (anthropicUsageFiveHourBudgetTokens config)
          entries
      weeklyWindow =
        buildRollingWindow
          now
          "7d"
          (7 * 24 * 60 * 60)
          (anthropicUsageWeeklyBudgetTokens config)
          entries
  return $
    AnthropicUsageInfo
      { anthropicUsageGeneratedAt = now,
        anthropicUsageSubscriptionType = anthropicMetadataSubscriptionType metadata,
        anthropicUsageRateLimitTier = anthropicMetadataRateLimitTier metadata,
        anthropicUsageHasAvailableSubscription = anthropicMetadataHasAvailableSubscription metadata,
        anthropicUsageExtraUsageDisabledReason = anthropicMetadataExtraUsageDisabledReason metadata,
        anthropicUsageOrganizationName = anthropicMetadataOrganizationName metadata,
        anthropicUsageFiveHourWindow = fiveHourWindow,
        anthropicUsageWeeklyWindow = weeklyWindow
      }

updateAnthropicUsage :: AnthropicUsageConfig -> IO AnthropicUsageSnapshot
updateAnthropicUsage config = do
  result <- try $ getAnthropicUsageInfo config
  case result of
    Right info -> return $ AnthropicUsageAvailable info
    Left (err :: SomeException) -> do
      let message = T.pack $ show err
      logM logName WARNING $ "Anthropic usage update failed: " <> show err
      return $ AnthropicUsageUnavailable message

readAnthropicUsageMetadata :: AnthropicUsageConfig -> IO AnthropicUsageMetadata
readAnthropicUsageMetadata config = do
  credentials <- decodeFileIfExists =<< maybe defaultClaudeCredentialsPath return (anthropicUsageCredentialsPath config)
  state <- decodeFileIfExists =<< maybe defaultClaudeStatePath return (anthropicUsageStatePath config)
  return $
    AnthropicUsageMetadata
      { anthropicMetadataSubscriptionType = credentials >>= anthropicCredentialsSubscriptionType,
        anthropicMetadataRateLimitTier = credentials >>= anthropicCredentialsRateLimitTier,
        anthropicMetadataHasAvailableSubscription = state >>= anthropicStateHasAvailableSubscription,
        anthropicMetadataExtraUsageDisabledReason = state >>= anthropicStateExtraUsageDisabledReason,
        anthropicMetadataOrganizationName = state >>= anthropicStateOrganizationName
      }

decodeFileIfExists :: (FromJSON a) => FilePath -> IO (Maybe a)
decodeFileIfExists path = do
  exists <- doesFileExist path
  if exists
    then do
      bytes <- LBS.readFile path
      case eitherDecode bytes of
        Right value -> return $ Just value
        Left err -> fail $ "Unable to parse " <> path <> ": " <> err
    else return Nothing

defaultClaudeStatePath :: IO FilePath
defaultClaudeStatePath = do
  home <- getHomeDirectory
  return $ home </> ".claude.json"

defaultClaudeCredentialsPath :: IO FilePath
defaultClaudeCredentialsPath = do
  home <- getHomeDirectory
  return $ home </> ".claude" </> ".credentials.json"

defaultClaudeProjectsPath :: IO FilePath
defaultClaudeProjectsPath = do
  home <- getHomeDirectory
  return $ home </> ".claude" </> "projects"

readAnthropicTranscriptEntries :: AnthropicUsageConfig -> UTCTime -> IO [TranscriptUsageEntry]
readAnthropicTranscriptEntries config now = do
  projectsPath <- maybe defaultClaudeProjectsPath return (anthropicUsageProjectsPath config)
  exists <- doesDirectoryExist projectsPath
  if not exists
    then return []
    else do
      files <- jsonlFilesModifiedSince (addUTCTime (negate $ anthropicUsageFileLookbackSeconds config) now) projectsPath
      dedupeTranscriptEntries . concat <$> mapM readTranscriptFile files

jsonlFilesModifiedSince :: UTCTime -> FilePath -> IO [FilePath]
jsonlFilesModifiedSince cutoff path = do
  entries <- listDirectory path
  concat
    <$> mapM
      ( \entry -> do
          let child = path </> entry
          isDirectory <- doesDirectoryExist child
          isFile <- doesFileExist child
          if isDirectory
            then jsonlFilesModifiedSince cutoff child
            else
              if isFile && takeExtension child == ".jsonl"
                then do
                  modified <- getModificationTime child
                  return [child | modified >= cutoff]
                else return []
      )
      entries

readTranscriptFile :: FilePath -> IO [TranscriptUsageEntry]
readTranscriptFile path = do
  bytes <- LBS.readFile path
  return $
    mapMaybe
      (uncurry $ transcriptLineToEntry path)
      (zip [(1 :: Int) ..] (LBS.lines bytes))

transcriptLineToEntry :: FilePath -> Int -> LBS.ByteString -> Maybe TranscriptUsageEntry
transcriptLineToEntry path lineNumber bytes = do
  decoded <- either (const Nothing) Just $ eitherDecode bytes
  timestamp <- transcriptJSONTimestamp decoded
  usage <- transcriptJSONUsage decoded
  let requestId =
        fromMaybe
          (T.pack $ path <> ":" <> show lineNumber)
          (transcriptJSONRequestId decoded)
      usageWithCost =
        usage
          { anthropicUsageEstimatedCostUSD =
              transcriptJSONCost decoded
                <|> anthropicUsageEstimatedCostUSD usage
          }
  return $
    TranscriptUsageEntry
      { transcriptUsageTimestamp = timestamp,
        transcriptUsageRequestId = requestId,
        transcriptUsageTotals = usageWithCost
      }

dedupeTranscriptEntries :: [TranscriptUsageEntry] -> [TranscriptUsageEntry]
dedupeTranscriptEntries =
  snd
    . foldl'
      ( \(seen, entries) entry ->
          let requestId = transcriptUsageRequestId entry
           in if Set.member requestId seen
                then (seen, entries)
                else (Set.insert requestId seen, entry : entries)
      )
      (Set.empty, [])

buildActiveBlockWindow ::
  UTCTime ->
  T.Text ->
  NominalDiffTime ->
  Maybe Int ->
  [TranscriptUsageEntry] ->
  AnthropicUsageWindow
buildActiveBlockWindow now name duration budget entries =
  case activeBlock now duration entries of
    Nothing ->
      AnthropicUsageWindow
        { anthropicUsageWindowName = name,
          anthropicUsageWindowStart = now,
          anthropicUsageWindowEnd = addUTCTime duration now,
          anthropicUsageWindowBudgetTokens = budget,
          anthropicUsageWindowTotals = mempty
        }
    Just (start, blockEntries) ->
      AnthropicUsageWindow
        { anthropicUsageWindowName = name,
          anthropicUsageWindowStart = start,
          anthropicUsageWindowEnd = addUTCTime duration start,
          anthropicUsageWindowBudgetTokens = budget,
          anthropicUsageWindowTotals = foldMap transcriptUsageTotals blockEntries
        }

activeBlock :: UTCTime -> NominalDiffTime -> [TranscriptUsageEntry] -> Maybe (UTCTime, [TranscriptUsageEntry])
activeBlock now duration entries =
  case foldl' addToBlock Nothing (sortOn transcriptUsageTimestamp entries) of
    Nothing -> Nothing
    Just (start, blockEntries)
      | addUTCTime duration start >= now -> Just (start, blockEntries)
      | otherwise -> Nothing
  where
    addToBlock Nothing entry =
      Just (transcriptUsageTimestamp entry, [entry])
    addToBlock (Just (start, blockEntries)) entry
      | transcriptUsageTimestamp entry < addUTCTime duration start =
          Just (start, blockEntries <> [entry])
      | otherwise =
          Just (transcriptUsageTimestamp entry, [entry])

buildRollingWindow ::
  UTCTime ->
  T.Text ->
  NominalDiffTime ->
  Maybe Int ->
  [TranscriptUsageEntry] ->
  AnthropicUsageWindow
buildRollingWindow now name duration budget entries =
  let start = addUTCTime (negate duration) now
      windowEntries = filter ((>= start) . transcriptUsageTimestamp) entries
      windowStart = maybe start transcriptUsageTimestamp $ firstMaybe $ sortOn transcriptUsageTimestamp windowEntries
      windowEnd = addUTCTime duration windowStart
   in AnthropicUsageWindow
        { anthropicUsageWindowName = name,
          anthropicUsageWindowStart = windowStart,
          anthropicUsageWindowEnd = windowEnd,
          anthropicUsageWindowBudgetTokens = budget,
          anthropicUsageWindowTotals = foldMap transcriptUsageTotals windowEntries
        }

firstMaybe :: [a] -> Maybe a
firstMaybe [] = Nothing
firstMaybe (x : _) = Just x

newtype AnthropicUsageChanVar
  = AnthropicUsageChanVar
      ( TChan AnthropicUsageSnapshot,
        MVar AnthropicUsageSnapshot,
        TChan ()
      )

getAnthropicUsageChan :: AnthropicUsageConfig -> TaffyIO (TChan AnthropicUsageSnapshot)
getAnthropicUsageChan config = do
  AnthropicUsageChanVar (chan, _, _) <- setupAnthropicUsageChanVar config
  return chan

getAnthropicUsageState :: AnthropicUsageConfig -> TaffyIO AnthropicUsageSnapshot
getAnthropicUsageState config = do
  AnthropicUsageChanVar (_, var, _) <- setupAnthropicUsageChanVar config
  liftIO $ readMVar var

forceAnthropicUsageRefresh :: AnthropicUsageConfig -> TaffyIO ()
forceAnthropicUsageRefresh config = do
  AnthropicUsageChanVar (_, _, refreshChan) <- setupAnthropicUsageChanVar config
  liftIO $ atomically $ writeTChan refreshChan ()

setupAnthropicUsageChanVar :: AnthropicUsageConfig -> TaffyIO AnthropicUsageChanVar
setupAnthropicUsageChanVar config = getStateDefault $ do
  chan <- liftIO newBroadcastTChanIO
  refreshChan <- liftIO newTChanIO
  initial <- liftIO $ updateAnthropicUsage config
  var <- liftIO $ newMVar initial
  wakeupChan <- getWakeupChannelForDelay (anthropicUsagePollInterval config)
  ourWakeupChan <- liftIO $ atomically $ dupTChan wakeupChan
  void $
    liftIO $
      forkIO $
        forever $ do
          atomically $
            void (readTChan refreshChan)
              `orElse` void (readTChan ourWakeupChan)
          refreshAnthropicUsageState config chan var
  return $ AnthropicUsageChanVar (chan, var, refreshChan)

refreshAnthropicUsageState ::
  AnthropicUsageConfig ->
  TChan AnthropicUsageSnapshot ->
  MVar AnthropicUsageSnapshot ->
  IO ()
refreshAnthropicUsageState config chan var = do
  snapshot <- updateAnthropicUsage config
  void $ swapMVar var snapshot
  atomically $ writeTChan chan snapshot

logName :: String
logName = "System.Taffybar.Information.AnthropicUsage"
