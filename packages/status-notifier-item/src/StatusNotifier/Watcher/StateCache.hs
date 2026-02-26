module StatusNotifier.Watcher.StateCache
  ( PersistedItemEntry (..)
  , PersistedWatcherState (..)
  , defaultWatcherStateCachePath
  , readPersistedWatcherState
  , writePersistedWatcherState
  ) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Data.Char (isAlphaNum, isDigit)
import Data.List (intercalate)
import System.Directory
  ( XdgDirectory (XdgCache)
  , createDirectoryIfMissing
  , doesFileExist
  , getXdgDirectory
  , renameFile
  )
import System.FilePath ((</>), takeDirectory)
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , choice
  , eof
  , many
  , munch
  , readP_to_S
  , satisfy
  , sepBy
  , skipSpaces
  )

data PersistedItemEntry = PersistedItemEntry
  { persistedServiceName :: String
  , persistedServicePath :: String
  } deriving (Eq, Show)

data PersistedWatcherState = PersistedWatcherState
  { persistedItems :: [PersistedItemEntry]
  , persistedHosts :: [PersistedItemEntry]
  } deriving (Eq, Show)

data JsonValue
  = JsonObject [(String, JsonValue)]
  | JsonArray [JsonValue]
  | JsonString String
  | JsonNumber Int
  deriving (Eq, Show)

defaultWatcherStateCachePath :: String -> String -> IO FilePath
defaultWatcherStateCachePath interfaceNamespace path = do
  xdgCachePath <- getXdgDirectory XdgCache "status-notifier-item"
  let sanitize = map (\c -> if isAlphaNum c then c else '_')
      filename =
        "watcher-state-"
          ++ sanitize interfaceNamespace
          ++ "-"
          ++ sanitize path
          ++ ".json"
  pure $ xdgCachePath </> filename

writePersistedWatcherState :: FilePath -> PersistedWatcherState -> IO (Either String ())
writePersistedWatcherState cachePath state = do
  let tempPath = cachePath ++ ".tmp"
      encodedState = encodePersistedWatcherState state
  writeResult <-
    try $ do
      createDirectoryIfMissing True (takeDirectory cachePath)
      writeFile tempPath encodedState
      renameFile tempPath cachePath
  pure $
    case writeResult of
      Left err -> Left (show (err :: IOException))
      Right _ -> Right ()

readPersistedWatcherState :: FilePath -> IO (Either String (Maybe PersistedWatcherState))
readPersistedWatcherState cachePath = do
  exists <- doesFileExist cachePath
  if not exists
    then pure $ Right Nothing
    else do
      readResult <- try (readFile cachePath) :: IO (Either IOException String)
      pure $
        case readResult of
          Left err -> Left (show err)
          Right contents -> Just <$> decodePersistedWatcherState contents

encodePersistedWatcherState :: PersistedWatcherState -> String
encodePersistedWatcherState state =
  encodeJsonValue $
    JsonObject
      [ ("version", JsonNumber 1)
      , ("items", JsonArray $ map encodeItemEntry (persistedItems state))
      , ("hosts", JsonArray $ map encodeItemEntry (persistedHosts state))
      ]

encodeItemEntry :: PersistedItemEntry -> JsonValue
encodeItemEntry entry =
  JsonObject
    [ ("service_name", JsonString (persistedServiceName entry))
    , ("service_path", JsonString (persistedServicePath entry))
    ]

decodePersistedWatcherState :: String -> Either String PersistedWatcherState
decodePersistedWatcherState raw = do
  root <- parseJson raw
  rootObject <- asObject "root" root
  version <- getRequiredNumber "version" rootObject
  if version /= 1
    then Left "Unsupported watcher cache version."
    else do
      itemsValue <- getRequired "items" rootObject
      hostsValue <- getRequired "hosts" rootObject
      items <- asArray "items" itemsValue >>= mapM decodeItemEntry
      hosts <- asArray "hosts" hostsValue >>= mapM decodeItemEntry
      Right PersistedWatcherState
        { persistedItems = items
        , persistedHosts = hosts
        }

decodeItemEntry :: JsonValue -> Either String PersistedItemEntry
decodeItemEntry value = do
  obj <- asObject "entry" value
  serviceName <- getRequiredString "service_name" obj
  servicePath <- getRequiredString "service_path" obj
  Right PersistedItemEntry
    { persistedServiceName = serviceName
    , persistedServicePath = servicePath
    }

encodeJsonValue :: JsonValue -> String
encodeJsonValue json =
  case json of
    JsonObject fields ->
      "{"
        ++ intercalate "," (map encodeField fields)
        ++ "}"
      where
        encodeField (name, value) =
          encodeJsonString name ++ ":" ++ encodeJsonValue value
    JsonArray values ->
      "[" ++ intercalate "," (map encodeJsonValue values) ++ "]"
    JsonString value ->
      encodeJsonString value
    JsonNumber number ->
      show number

encodeJsonString :: String -> String
encodeJsonString value =
  "\"" ++ concatMap encodeChar value ++ "\""
  where
    encodeChar '"' = "\\\""
    encodeChar '\\' = "\\\\"
    encodeChar '\n' = "\\n"
    encodeChar '\r' = "\\r"
    encodeChar '\t' = "\\t"
    encodeChar c = [c]

parseJson :: String -> Either String JsonValue
parseJson input =
  case [value | (value, rest) <- readP_to_S parseJsonDocument input, null rest] of
    [value] -> Right value
    [] -> Left "Failed to parse watcher cache JSON."
    _ -> Left "Watcher cache JSON is ambiguous."

parseJsonDocument :: ReadP JsonValue
parseJsonDocument = do
  skipSpaces
  value <- parseJsonValue
  skipSpaces
  eof
  return value

parseJsonValue :: ReadP JsonValue
parseJsonValue = do
  skipSpaces
  choice [parseJsonObject, parseJsonArray, JsonString <$> parseJsonString, parseJsonNumber]

parseJsonObject :: ReadP JsonValue
parseJsonObject = do
  _ <- char '{'
  skipSpaces
  fields <- sepBy parseJsonField (skipSpaces >> char ',' >> skipSpaces)
  skipSpaces
  _ <- char '}'
  return $ JsonObject fields

parseJsonField :: ReadP (String, JsonValue)
parseJsonField = do
  key <- parseJsonString
  skipSpaces
  _ <- char ':'
  skipSpaces
  value <- parseJsonValue
  return (key, value)

parseJsonArray :: ReadP JsonValue
parseJsonArray = do
  _ <- char '['
  skipSpaces
  values <- sepBy parseJsonValue (skipSpaces >> char ',' >> skipSpaces)
  skipSpaces
  _ <- char ']'
  return $ JsonArray values

parseJsonString :: ReadP String
parseJsonString = do
  _ <- char '"'
  chars <- many parseJsonStringChar
  _ <- char '"'
  return chars

parseJsonStringChar :: ReadP Char
parseJsonStringChar =
  parseEscaped <|> parseUnescaped
  where
    parseEscaped = do
      _ <- char '\\'
      choice
        [ '"' <$ char '"'
        , '\\' <$ char '\\'
        , '\n' <$ char 'n'
        , '\r' <$ char 'r'
        , '\t' <$ char 't'
        ]
    parseUnescaped = satisfy (\c -> c /= '"' && c /= '\\')

parseJsonNumber :: ReadP JsonValue
parseJsonNumber = do
  sign <- munch (\c -> c == '-')
  digits <- munch isDigit
  if null digits || length sign > 1
    then fail "Invalid number"
    else return $ JsonNumber (read (sign ++ digits))

getRequired :: String -> [(String, JsonValue)] -> Either String JsonValue
getRequired key objectFields =
  case lookup key objectFields of
    Nothing -> Left $ "Missing key: " ++ key
    Just value -> Right value

getRequiredString :: String -> [(String, JsonValue)] -> Either String String
getRequiredString key fields = do
  value <- getRequired key fields
  asString key value

getRequiredNumber :: String -> [(String, JsonValue)] -> Either String Int
getRequiredNumber key fields = do
  value <- getRequired key fields
  asNumber key value

asObject :: String -> JsonValue -> Either String [(String, JsonValue)]
asObject label value =
  case value of
    JsonObject fields -> Right fields
    _ -> Left $ "Expected object for " ++ label

asArray :: String -> JsonValue -> Either String [JsonValue]
asArray label value =
  case value of
    JsonArray values -> Right values
    _ -> Left $ "Expected array for " ++ label

asString :: String -> JsonValue -> Either String String
asString label value =
  case value of
    JsonString str -> Right str
    _ -> Left $ "Expected string for " ++ label

asNumber :: String -> JsonValue -> Either String Int
asNumber label value =
  case value of
    JsonNumber number -> Right number
    _ -> Left $ "Expected number for " ++ label
