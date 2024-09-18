{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.Auth where

import           Control.Monad.IO.Class
import           Data.Maybe
import           System.Taffybar.Util
import           Text.Regex

fieldRegex :: Regex
fieldRegex = mkRegexWithOpts "^(.*?): *(.*?)$" True True

passGet :: MonadIO m => String -> m (Either String (String, [(String, String)]))
passGet credentialName = (>>= getPassComponents . lines) <$> runPassShow
  where runPassShow = runCommandFromPath ["pass", "show", credentialName]

        getPassComponents [] = Left "pass show command produced no output"
        getPassComponents (key:rest) = Right (key, buildEntries rest)

        buildEntries = mapMaybe buildEntry . mapMaybe (matchRegex fieldRegex)

        buildEntry [fieldName, fieldValue] = Just (fieldName, fieldValue)
        buildEntry _ = Nothing
