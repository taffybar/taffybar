{-# LANGUAGE OverloadedStrings #-}

-- | Shared Claude Code subscription usage information.
--
-- Rate-limit utilization percentages and reset times come from the OAuth usage
-- endpoint that Claude Code itself polls, authenticated with the access token
-- from Claude Code's local credentials file. Token totals are still derived
-- from the local transcript JSONL files under @~/.claude/projects@, and serve
-- as a fallback display when the endpoint is unreachable.
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
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Network.HTTP.Simple
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getFileSize,
    getHomeDirectory,
    getModificationTime,
    listDirectory,
  )
import System.FilePath (takeExtension, (</>))
import System.Log.Logger (Priority (WARNING), logM)
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Information.Wakeup (getWakeupChannelForDelay)
import Text.Read (readMaybe)

data AnthropicUsageConfig = AnthropicUsageConfig
  { -- | How often the poll loop wakes. Drives the cheap local transcript read;
    -- the rate-limited OAuth endpoint is governed separately by
    -- 'anthropicUsageOAuthMinInterval' and the backoff state, so this can be
    -- set low for snappy local token counts without abusing the endpoint.
    anthropicUsagePollInterval :: Double,
    anthropicUsageEndpoint :: String,
    anthropicUsageUserAgent :: String,
    -- | Minimum spacing between successful OAuth endpoint fetches. Even if the
    -- poll loop wakes more often, the endpoint is not hit again until this much
    -- time has elapsed since the last success.
    anthropicUsageOAuthMinInterval :: NominalDiffTime,
    -- | Upper bound on the exponential backoff applied after failed OAuth
    -- fetches (429s, auth errors, network errors). A server-supplied
    -- @Retry-After@ longer than this is still honored.
    anthropicUsageOAuthMaxBackoff :: NominalDiffTime,
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
      anthropicUsageEndpoint = "https://api.anthropic.com/api/oauth/usage",
      anthropicUsageUserAgent = "taffybar-anthropic-usage",
      anthropicUsageOAuthMinInterval = 60 * 5,
      anthropicUsageOAuthMaxBackoff = 60 * 60,
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
    anthropicUsageWeeklyWindow :: AnthropicUsageWindow,
    -- | Weekly limit scoped to a single model (currently \"Fable\"), reported
    -- separately by the OAuth usage endpoint. 'Nothing' when the plan has no
    -- such per-model weekly bucket. The window's name carries the model's
    -- display name so the label reads e.g. @Fable 22%@.
    anthropicUsageScopedWeeklyWindow :: Maybe AnthropicUsageWindow
  }
  deriving (Eq, Show)

data AnthropicUsageWindow = AnthropicUsageWindow
  { anthropicUsageWindowName :: T.Text,
    anthropicUsageWindowStart :: UTCTime,
    anthropicUsageWindowEnd :: UTCTime,
    -- | Authoritative reset time reported by the OAuth usage endpoint. The
    -- fallback transcript windows synthesize an end time, but do not claim to
    -- know when Claude's server-side limit resets.
    anthropicUsageWindowResetAt :: Maybe UTCTime,
    anthropicUsageWindowBudgetTokens :: Maybe Int,
    -- | Used percentage reported by the OAuth usage endpoint, when available.
    anthropicUsageWindowUtilizationPercent :: Maybe Double,
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
    anthropicCredentialsRateLimitTier :: Maybe T.Text,
    anthropicCredentialsAccessToken :: Maybe T.Text
  }

instance FromJSON AnthropicCredentials where
  parseJSON = withObject "AnthropicCredentials" $ \root -> do
    oauth <- root .:? "claudeAiOauth"
    case oauth of
      Nothing -> return $ AnthropicCredentials Nothing Nothing Nothing
      Just value ->
        withObject
          "ClaudeAiOauth"
          ( \o ->
              AnthropicCredentials
                <$> o .:? "subscriptionType"
                <*> o .:? "rateLimitTier"
                <*> o .:? "accessToken"
          )
          value

-- | Window utilization as reported by the OAuth usage endpoint.
data AnthropicOAuthWindow = AnthropicOAuthWindow
  { anthropicOAuthWindowUtilization :: Maybe Double,
    anthropicOAuthWindowResetsAt :: Maybe UTCTime
  }
  deriving (Eq, Show)

instance FromJSON AnthropicOAuthWindow where
  parseJSON = withObject "AnthropicOAuthWindow" $ \o ->
    AnthropicOAuthWindow
      <$> o .:? "utilization"
      <*> o .:? "resets_at"

-- | A single entry from the OAuth usage endpoint's @limits@ array. Used to
-- surface the per-model @weekly_scoped@ limit (e.g. Fable), which has no
-- dedicated top-level field.
data AnthropicOAuthLimit = AnthropicOAuthLimit
  { anthropicOAuthLimitKind :: Maybe T.Text,
    anthropicOAuthLimitPercent :: Maybe Double,
    anthropicOAuthLimitResetsAt :: Maybe UTCTime,
    -- | @scope.model.display_name@, when the limit is scoped to a model.
    anthropicOAuthLimitScopeModel :: Maybe T.Text
  }
  deriving (Eq, Show)

instance FromJSON AnthropicOAuthLimit where
  parseJSON = withObject "AnthropicOAuthLimit" $ \o -> do
    scope <- o .:? "scope"
    scopeModel <-
      maybe
        (return Nothing)
        ( withObject "Scope" $ \s -> do
            model <- s .:? "model"
            maybe
              (return Nothing)
              (withObject "Model" (.:? "display_name"))
              model
        )
        scope
    AnthropicOAuthLimit
      <$> o .:? "kind"
      <*> o .:? "percent"
      <*> o .:? "resets_at"
      <*> pure scopeModel

data AnthropicOAuthUsage = AnthropicOAuthUsage
  { anthropicOAuthFiveHour :: Maybe AnthropicOAuthWindow,
    anthropicOAuthSevenDay :: Maybe AnthropicOAuthWindow,
    anthropicOAuthLimits :: [AnthropicOAuthLimit]
  }
  deriving (Eq, Show)

instance FromJSON AnthropicOAuthUsage where
  parseJSON = withObject "AnthropicOAuthUsage" $ \o ->
    AnthropicOAuthUsage
      <$> o .:? "five_hour"
      <*> o .:? "seven_day"
      <*> o .:? "limits" .!= []

-- | Persistent state for OAuth endpoint polling, carried across poll
-- iterations so we can space out and back off endpoint requests independently
-- of the (cheap, local) transcript reads.
data OAuthFetchState = OAuthFetchState
  { -- | Earliest time we are allowed to hit the endpoint again. 'Nothing'
    -- means "fetch on next poll".
    oauthNextAllowedFetch :: Maybe UTCTime,
    -- | Consecutive failures, used to grow the exponential backoff.
    oauthConsecutiveFailures :: Int,
    -- | Last successful response, shown while we are within a backoff/spacing
    -- window so the widget keeps real utilization instead of dropping to the
    -- transcript-derived fallback.
    oauthLastGood :: Maybe AnthropicOAuthUsage,
    -- | Access token used on the last attempt. A changed token (e.g. after
    -- Claude Code refreshes its credentials) bypasses the backoff so a
    -- recovered token is picked up immediately rather than after a long wait.
    oauthLastTriedToken :: Maybe T.Text
  }

emptyOAuthFetchState :: OAuthFetchState
emptyOAuthFetchState = OAuthFetchState Nothing 0 Nothing Nothing

-- | Outcome of a single HTTP attempt against the OAuth usage endpoint.
data OAuthFetchResult
  = -- | 2xx with a decodable body.
    OAuthFetchSuccess AnthropicOAuthUsage
  | -- | HTTP 429, with the parsed @Retry-After@ delay when present.
    OAuthFetchRateLimited (Maybe NominalDiffTime)
  | -- | Any other non-success (auth error, 5xx, decode failure, network error).
    OAuthFetchError

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

-- | Cached extraction result for a single transcript file, keyed by the
-- file's modification time and size. Only the extracted usage entries are
-- retained, never file contents, so the cache stays small even though the
-- underlying transcripts can be large.
data TranscriptFileCacheEntry = TranscriptFileCacheEntry
  { transcriptCacheModified :: UTCTime,
    transcriptCacheSize :: Integer,
    transcriptCacheEntries :: [TranscriptUsageEntry]
  }

-- | Per-file cache of transcript extractions, carried across poll iterations
-- so unchanged files are not re-read and re-parsed on every poll.
type TranscriptFileCache = Map.Map FilePath TranscriptFileCacheEntry

emptyTranscriptFileCache :: TranscriptFileCache
emptyTranscriptFileCache = Map.empty

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

-- | One-shot snapshot with no persistent backoff or transcript cache state.
-- Convenience for callers and tests; the poll loop uses
-- 'getAnthropicUsageInfoWith' so backoff, last-good caching, and the per-file
-- transcript cache survive across iterations.
getAnthropicUsageInfo :: AnthropicUsageConfig -> IO AnthropicUsageInfo
getAnthropicUsageInfo config = do
  oauthStateRef <- newIORef emptyOAuthFetchState
  transcriptCacheRef <- newIORef emptyTranscriptFileCache
  getAnthropicUsageInfoWith config oauthStateRef transcriptCacheRef

getAnthropicUsageInfoWith :: AnthropicUsageConfig -> IORef OAuthFetchState -> IORef TranscriptFileCache -> IO AnthropicUsageInfo
getAnthropicUsageInfoWith config oauthStateRef transcriptCacheRef = do
  now <- getCurrentTime
  credentials <- decodeFileIfExists =<< maybe defaultClaudeCredentialsPath return (anthropicUsageCredentialsPath config)
  metadata <- readAnthropicUsageMetadata config credentials
  oauthUsage <- fetchAnthropicOAuthUsageWithBackoff config oauthStateRef now (credentials >>= anthropicCredentialsAccessToken)
  entries <- readAnthropicTranscriptEntries config transcriptCacheRef now
  let fiveHourWindow =
        applyOAuthWindow (oauthUsage >>= anthropicOAuthFiveHour) $
          buildActiveBlockWindow
            now
            "5h"
            (5 * 60 * 60)
            (anthropicUsageFiveHourBudgetTokens config)
            entries
      weeklyWindow =
        applyOAuthWindow (oauthUsage >>= anthropicOAuthSevenDay) $
          buildRollingWindow
            now
            "7d"
            (7 * 24 * 60 * 60)
            (anthropicUsageWeeklyBudgetTokens config)
            entries
      scopedWeeklyWindow =
        buildScopedWeeklyWindow now
          <$> (oauthUsage >>= findScopedWeeklyLimit)
  return $
    AnthropicUsageInfo
      { anthropicUsageGeneratedAt = now,
        anthropicUsageSubscriptionType = anthropicMetadataSubscriptionType metadata,
        anthropicUsageRateLimitTier = anthropicMetadataRateLimitTier metadata,
        anthropicUsageHasAvailableSubscription = anthropicMetadataHasAvailableSubscription metadata,
        anthropicUsageExtraUsageDisabledReason = anthropicMetadataExtraUsageDisabledReason metadata,
        anthropicUsageOrganizationName = anthropicMetadataOrganizationName metadata,
        anthropicUsageFiveHourWindow = fiveHourWindow,
        anthropicUsageWeeklyWindow = weeklyWindow,
        anthropicUsageScopedWeeklyWindow = scopedWeeklyWindow
      }

updateAnthropicUsage :: AnthropicUsageConfig -> IO AnthropicUsageSnapshot
updateAnthropicUsage config = do
  oauthStateRef <- newIORef emptyOAuthFetchState
  transcriptCacheRef <- newIORef emptyTranscriptFileCache
  updateAnthropicUsageWith config oauthStateRef transcriptCacheRef

updateAnthropicUsageWith :: AnthropicUsageConfig -> IORef OAuthFetchState -> IORef TranscriptFileCache -> IO AnthropicUsageSnapshot
updateAnthropicUsageWith config oauthStateRef transcriptCacheRef = do
  result <- try $ getAnthropicUsageInfoWith config oauthStateRef transcriptCacheRef
  case result of
    Right info -> return $ AnthropicUsageAvailable info
    Left (err :: SomeException) -> do
      let message = T.pack $ show err
      logM logName WARNING $ "Anthropic usage update failed: " <> show err
      return $ AnthropicUsageUnavailable message

readAnthropicUsageMetadata :: AnthropicUsageConfig -> Maybe AnthropicCredentials -> IO AnthropicUsageMetadata
readAnthropicUsageMetadata config credentials = do
  state <- decodeFileIfExists =<< maybe defaultClaudeStatePath return (anthropicUsageStatePath config)
  return $
    AnthropicUsageMetadata
      { anthropicMetadataSubscriptionType = credentials >>= anthropicCredentialsSubscriptionType,
        anthropicMetadataRateLimitTier = credentials >>= anthropicCredentialsRateLimitTier,
        anthropicMetadataHasAvailableSubscription = state >>= anthropicStateHasAvailableSubscription,
        anthropicMetadataExtraUsageDisabledReason = state >>= anthropicStateExtraUsageDisabledReason,
        anthropicMetadataOrganizationName = state >>= anthropicStateOrganizationName
      }

-- | Fetch utilization from the OAuth usage endpoint, honoring a persistent
-- backoff carried in the 'IORef'. Returns the value to display: a fresh
-- response on success, or the last good response while spacing out / backing
-- off, or 'Nothing' when nothing is cached (the widget then falls back to
-- transcript-derived token counts).
--
-- The endpoint is rate-limited, so we (a) never hit it more often than
-- 'anthropicUsageOAuthMinInterval' after a success, (b) apply exponential
-- backoff (honoring a server @Retry-After@) after failures, and (c) bypass the
-- backoff when the access token changes, so a refreshed credential recovers
-- immediately instead of after the full backoff.
fetchAnthropicOAuthUsageWithBackoff ::
  AnthropicUsageConfig ->
  IORef OAuthFetchState ->
  UTCTime ->
  Maybe T.Text ->
  IO (Maybe AnthropicOAuthUsage)
fetchAnthropicOAuthUsageWithBackoff _ _ _ Nothing = return Nothing
fetchAnthropicOAuthUsageWithBackoff config oauthStateRef now (Just token) = do
  st <- readIORef oauthStateRef
  let tokenChanged = oauthLastTriedToken st /= Just token
      withinBackoff = maybe False (now <) (oauthNextAllowedFetch st)
      -- A changed token restarts the failure schedule from scratch.
      priorFailures = if tokenChanged then 0 else oauthConsecutiveFailures st
  if withinBackoff && not tokenChanged
    then return (oauthLastGood st)
    else do
      outcome <- performOAuthFetch config token
      case outcome of
        OAuthFetchSuccess usage -> do
          writeIORef oauthStateRef $
            st
              { oauthConsecutiveFailures = 0,
                oauthLastGood = Just usage,
                oauthLastTriedToken = Just token,
                oauthNextAllowedFetch =
                  Just (addUTCTime (anthropicUsageOAuthMinInterval config) now)
              }
          return (Just usage)
        OAuthFetchRateLimited retryAfter ->
          backOff st (priorFailures + 1) retryAfter
        OAuthFetchError ->
          backOff st (priorFailures + 1) Nothing
  where
    backOff st failures retryAfter = do
      let delay = oauthBackoffDelay config failures retryAfter
      writeIORef oauthStateRef $
        st
          { oauthConsecutiveFailures = failures,
            oauthLastTriedToken = Just token,
            oauthNextAllowedFetch = Just (addUTCTime delay now)
          }
      return (oauthLastGood st)

-- | A single HTTP attempt against the OAuth usage endpoint, classified into an
-- 'OAuthFetchResult'. Network and decode errors are logged and reported as
-- 'OAuthFetchError'.
performOAuthFetch :: AnthropicUsageConfig -> T.Text -> IO OAuthFetchResult
performOAuthFetch config token = do
  result <- try $ do
    request0 <- parseRequest $ anthropicUsageEndpoint config
    let request =
          setRequestHeader "Authorization" ["Bearer " <> TE.encodeUtf8 token] $
            setRequestHeader "anthropic-beta" ["oauth-2025-04-20"] $
              setRequestHeader "Accept" ["application/json"] $
                setRequestHeader "User-Agent" [TE.encodeUtf8 (T.pack (anthropicUsageUserAgent config))] request0
    response <- httpLBS request
    return
      ( getResponseStatusCode response,
        getResponseHeader "Retry-After" response,
        getResponseBody response
      )
  case result of
    Left (err :: SomeException) -> do
      logM logName WARNING $ "Anthropic OAuth usage fetch failed: " <> show err
      return OAuthFetchError
    Right (statusCode, retryAfterHeaders, body)
      | statusCode >= 200 && statusCode < 300 ->
          case eitherDecode body of
            Right usage -> return (OAuthFetchSuccess usage)
            Left err -> do
              logM logName WARNING $ "Anthropic OAuth usage decode failed: " <> err
              return OAuthFetchError
      | statusCode == 429 -> do
          let retryAfter = parseRetryAfter retryAfterHeaders
          logM logName WARNING $
            "Anthropic usage endpoint returned HTTP 429"
              <> maybe
                ""
                (\s -> "; Retry-After " <> show (round s :: Integer) <> "s")
                retryAfter
          return (OAuthFetchRateLimited retryAfter)
      | otherwise -> do
          logM logName WARNING $ "Anthropic usage endpoint returned HTTP " <> show statusCode
          return OAuthFetchError

-- | Exponential backoff that doubles from the configured min interval and is
-- capped at the configured max, but is never shorter than a server-supplied
-- @Retry-After@.
oauthBackoffDelay :: AnthropicUsageConfig -> Int -> Maybe NominalDiffTime -> NominalDiffTime
oauthBackoffDelay config failures retryAfter =
  max (fromMaybe 0 retryAfter) capped
  where
    base = anthropicUsageOAuthMinInterval config
    shift = min (max 0 (failures - 1)) 6
    capped = min (anthropicUsageOAuthMaxBackoff config) (base * fromInteger (2 ^ shift))

-- | Parse a @Retry-After@ header expressed as integer delta-seconds. The
-- HTTP-date form is not handled and falls back to exponential backoff.
parseRetryAfter :: [BS8.ByteString] -> Maybe NominalDiffTime
parseRetryAfter headers = do
  raw <- listToMaybe headers
  seconds <- readMaybe (BS8.unpack raw) :: Maybe Integer
  if seconds >= 0 then Just (fromInteger seconds) else Nothing

-- | The first @weekly_scoped@ limit from the endpoint's @limits@ array, i.e.
-- the per-model weekly bucket (currently Fable). Only limits carrying a model
-- scope are considered, so an unscoped @weekly_scoped@ entry is ignored.
findScopedWeeklyLimit :: AnthropicOAuthUsage -> Maybe AnthropicOAuthLimit
findScopedWeeklyLimit usage =
  listToMaybe
    [ limit
    | limit <- anthropicOAuthLimits usage,
      anthropicOAuthLimitKind limit == Just "weekly_scoped",
      Just _ <- [anthropicOAuthLimitScopeModel limit]
    ]

-- | Build a display window for a per-model weekly limit. There is no
-- transcript-derived fallback for these, so the window is populated purely
-- from the endpoint: the model display name becomes the window name and the
-- reported percent becomes the utilization. The start is derived by walking
-- back a week from the reset time so menu durations read sensibly.
buildScopedWeeklyWindow :: UTCTime -> AnthropicOAuthLimit -> AnthropicUsageWindow
buildScopedWeeklyWindow now limit =
  AnthropicUsageWindow
    { anthropicUsageWindowName = fromMaybe "scoped" (anthropicOAuthLimitScopeModel limit),
      anthropicUsageWindowStart =
        maybe now (addUTCTime (negate weekSeconds)) resetsAt,
      anthropicUsageWindowEnd = fromMaybe (addUTCTime weekSeconds now) resetsAt,
      anthropicUsageWindowResetAt = resetsAt,
      anthropicUsageWindowBudgetTokens = Nothing,
      anthropicUsageWindowUtilizationPercent = anthropicOAuthLimitPercent limit,
      anthropicUsageWindowTotals = mempty
    }
  where
    resetsAt = anthropicOAuthLimitResetsAt limit
    weekSeconds = 7 * 24 * 60 * 60

applyOAuthWindow :: Maybe AnthropicOAuthWindow -> AnthropicUsageWindow -> AnthropicUsageWindow
applyOAuthWindow Nothing window = window
applyOAuthWindow (Just oauthWindow) window =
  window
    { anthropicUsageWindowUtilizationPercent = anthropicOAuthWindowUtilization oauthWindow,
      anthropicUsageWindowEnd =
        fromMaybe (anthropicUsageWindowEnd window) (anthropicOAuthWindowResetsAt oauthWindow),
      anthropicUsageWindowResetAt = anthropicOAuthWindowResetsAt oauthWindow
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

readAnthropicTranscriptEntries :: AnthropicUsageConfig -> IORef TranscriptFileCache -> UTCTime -> IO [TranscriptUsageEntry]
readAnthropicTranscriptEntries config transcriptCacheRef now = do
  projectsPath <- maybe defaultClaudeProjectsPath return (anthropicUsageProjectsPath config)
  exists <- doesDirectoryExist projectsPath
  if not exists
    then do
      writeIORef transcriptCacheRef emptyTranscriptFileCache
      return []
    else do
      files <- jsonlFilesModifiedSince (addUTCTime (negate $ anthropicUsageFileLookbackSeconds config) now) projectsPath
      cache <- readIORef transcriptCacheRef
      cachedFiles <- mapM (readTranscriptFileCached cache) files
      -- Rebuilding the map from this poll's scan evicts entries for files
      -- that disappeared or aged out of the lookback window.
      writeIORef transcriptCacheRef $ Map.fromList cachedFiles
      return $ dedupeTranscriptEntries $ concatMap (transcriptCacheEntries . snd) cachedFiles

jsonlFilesModifiedSince :: UTCTime -> FilePath -> IO [(FilePath, UTCTime)]
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
                  return [(child, modified) | modified >= cutoff]
                else return []
      )
      entries

-- | Return the cached extraction for a file when its modification time and
-- size are both unchanged, re-reading and re-parsing it otherwise. Freshly
-- parsed entries are forced before caching so no lingering thunks retain the
-- file's parsed contents.
readTranscriptFileCached ::
  TranscriptFileCache ->
  (FilePath, UTCTime) ->
  IO (FilePath, TranscriptFileCacheEntry)
readTranscriptFileCached cache (path, modified) = do
  size <- getFileSize path
  case Map.lookup path cache of
    Just cached
      | transcriptCacheModified cached == modified,
        transcriptCacheSize cached == size ->
          return (path, cached)
    _ -> do
      entries <- readTranscriptFile path
      mapM_ (evaluate . forceUsageEntry) entries
      return (path, TranscriptFileCacheEntry modified size entries)

-- | Fully evaluate an extracted entry so values held in the cache do not
-- retain thunks referencing the transcript file's parsed JSON.
forceUsageEntry :: TranscriptUsageEntry -> ()
forceUsageEntry
  ( TranscriptUsageEntry
      timestamp
      requestId
      (AnthropicUsageTotals requests input cacheCreation cacheRead output cost)
    ) =
    timestamp `seq`
      requestId `seq`
        requests `seq`
          input `seq`
            cacheCreation `seq`
              cacheRead `seq`
                output `seq`
                  maybe () (`seq` ()) cost

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
    . List.foldl'
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
          anthropicUsageWindowResetAt = Nothing,
          anthropicUsageWindowBudgetTokens = budget,
          anthropicUsageWindowUtilizationPercent = Nothing,
          anthropicUsageWindowTotals = mempty
        }
    Just (start, blockEntries) ->
      AnthropicUsageWindow
        { anthropicUsageWindowName = name,
          anthropicUsageWindowStart = start,
          anthropicUsageWindowEnd = addUTCTime duration start,
          anthropicUsageWindowResetAt = Nothing,
          anthropicUsageWindowBudgetTokens = budget,
          anthropicUsageWindowUtilizationPercent = Nothing,
          anthropicUsageWindowTotals = foldMap transcriptUsageTotals blockEntries
        }

activeBlock :: UTCTime -> NominalDiffTime -> [TranscriptUsageEntry] -> Maybe (UTCTime, [TranscriptUsageEntry])
activeBlock now duration entries =
  case List.foldl' addToBlock Nothing (sortOn transcriptUsageTimestamp entries) of
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
          anthropicUsageWindowResetAt = Nothing,
          anthropicUsageWindowBudgetTokens = budget,
          anthropicUsageWindowUtilizationPercent = Nothing,
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
  -- Persistent OAuth backoff/last-good state, shared by the initial fetch and
  -- every poll iteration so spacing and backoff survive across wakeups.
  oauthStateRef <- liftIO $ newIORef emptyOAuthFetchState
  -- Per-file transcript extraction cache, shared across poll iterations so
  -- unchanged transcript files are not re-read and re-parsed on every poll.
  transcriptCacheRef <- liftIO $ newIORef emptyTranscriptFileCache
  initial <- liftIO $ updateAnthropicUsageWith config oauthStateRef transcriptCacheRef
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
          refreshAnthropicUsageState config oauthStateRef transcriptCacheRef chan var
  return $ AnthropicUsageChanVar (chan, var, refreshChan)

refreshAnthropicUsageState ::
  AnthropicUsageConfig ->
  IORef OAuthFetchState ->
  IORef TranscriptFileCache ->
  TChan AnthropicUsageSnapshot ->
  MVar AnthropicUsageSnapshot ->
  IO ()
refreshAnthropicUsageState config oauthStateRef transcriptCacheRef chan var = do
  snapshot <- updateAnthropicUsageWith config oauthStateRef transcriptCacheRef
  void $ swapMVar var snapshot
  atomically $ writeTChan chan snapshot

logName :: String
logName = "System.Taffybar.Information.AnthropicUsage"
