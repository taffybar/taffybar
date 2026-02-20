{-# LANGUAGE OverloadedStrings #-}

-- | Helpers for retrieving and parsing credentials from the @pass@ password
-- store.
module System.Taffybar.Auth where

import Control.Monad.IO.Class
import Data.Maybe
import System.Taffybar.Util
import Text.Regex

-- | Regex for @field: value@ lines in @pass show@ output.
fieldRegex :: Regex
fieldRegex = mkRegexWithOpts "^(.*?): *(.*?)$" True True

-- | Read a secret with @pass show@ and parse additional fields from subsequent
-- lines.
--
-- Returns either an error string or @(primarySecret, extraFields)@.
passGet :: (MonadIO m) => String -> m (Either String (String, [(String, String)]))
passGet credentialName = (>>= getPassComponents . lines) <$> runPassShow
  where
    runPassShow = runCommand "pass" ["show", credentialName]

    getPassComponents [] = Left "pass show command produced no output"
    getPassComponents (key : rest) = Right (key, buildEntries rest)

    buildEntries = mapMaybe buildEntry . mapMaybe (matchRegex fieldRegex)

    buildEntry [fieldName, fieldValue] = Just (fieldName, fieldValue)
    buildEntry _ = Nothing
