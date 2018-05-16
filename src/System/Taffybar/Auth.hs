{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.Auth where

import           Control.Arrow
import           Control.Monad.IO.Class
import           Data.Maybe
import           System.Taffybar.Util
import           Text.Regex

fieldRegex :: Regex
fieldRegex = mkRegexWithOpts "^(.*?): *(.*?)$" True True

passGet :: MonadIO m => String -> m (Either String (String, [(String, String)]))
passGet credentialName =
  right (getPassComponents . lines) <$> runCommandFromPath ["pass", "show", credentialName]
  where getPassComponents passLines =
          let entries = map buildEntry $ catMaybes $ matchRegex fieldRegex <$> tail passLines
              buildEntry [fieldName, fieldValue] = (fieldName, fieldValue)
              buildEntry _ = ("", "")
          in (head passLines, entries)
