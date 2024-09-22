{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module System.Taffybar.AuthSpec (spec) where

import System.Taffybar.Auth
import System.Taffybar.SpecUtil (withMockCommand)
import Test.Hspec
import Test.Hspec.Golden
import Text.Printf

spec :: Spec
spec = aroundAll_ (withMockPass mockDb) $ describe "passGet" $ do
  golden "get a password" $ show <$> passGet "hello"
  golden "get a password with info" $ show <$> passGet "multiline"
  golden "missing entry" $ show <$> passGet "missing"

mockDb :: [MockEntry]
mockDb = [ mockEntry "hello" "xyzzy" []
         , mockEntry "multiline" "secret" [("Username", "fred"), ("silly", "")]
         , fallbackEntry "" "Error: is not in the password store.\n" 1
         ]

withMockPass :: [MockEntry] -> IO a -> IO a
withMockPass db = withMockCommand "pass" (mockScript db)

data MockEntry = MockEntry
  { passName :: String
  , out :: String
  , err :: String
  , status :: Int
  } deriving (Show, Read, Eq)

mockEntry :: String -> String -> [(String, String)] -> MockEntry
mockEntry passName key info =
  MockEntry { passName, out = passFile key info, err = "", status = 0 }

passFile :: String -> [(String, String)] -> String
passFile key info = unlines (key:[k ++ ": " ++ v | (k, v) <- info])

fallbackEntry :: String -> String -> Int -> MockEntry
fallbackEntry out err status = MockEntry { passName = "", .. }

mockScript :: [MockEntry] -> String
mockScript db = unlines ("#!/usr/bin/env bash":map makeEntry db)
  where
    makeEntry MockEntry{..} = printf template passName out err status
    template = unlines
      [ "pass_name='%s'"
      , "if [ -z \"$pass_name\" -o \"$2\" = \"$pass_name\" ]; then"
      , "  echo -n '%s'"
      , "  >&2 echo '%s'"
      , "  exit %d"
      , "fi"
      ]
