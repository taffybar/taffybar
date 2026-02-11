{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.Hyprland.Types
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Typed representations of JSON structures returned by @hyprctl -j@.
--
-- These are intended to be used by widgets so they do not need to deal with
-- raw JSON parsing and partial field lookups.
module System.Taffybar.Information.Hyprland.Types
  ( HyprlandWorkspaceRef (..),
    HyprlandWorkspaceInfo (..),
    HyprlandMonitorInfo (..),
    HyprlandClientInfo (..),
    HyprlandActiveWorkspaceInfo (..),
    HyprlandActiveWindowInfo (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Text (Text)

parseOptionalAt :: Maybe [Int] -> Parser (Maybe (Int, Int))
parseOptionalAt mAt =
  case mAt of
    Nothing -> pure Nothing
    Just [x, y] -> pure $ Just (x, y)
    Just _ -> fail "Expected \"at\" to be a 2-element array"

data HyprlandWorkspaceRef = HyprlandWorkspaceRef
  { hyprWorkspaceRefId :: Int,
    hyprWorkspaceRefName :: Text
  }
  deriving (Show, Eq)

instance FromJSON HyprlandWorkspaceRef where
  parseJSON = withObject "HyprlandWorkspaceRef" $ \v ->
    HyprlandWorkspaceRef
      <$> v .: "id"
      <*> v .: "name"

-- | Entry from @hyprctl -j workspaces@.
data HyprlandWorkspaceInfo = HyprlandWorkspaceInfo
  { hyprWorkspaceId :: Int,
    hyprWorkspaceName :: Text,
    hyprWorkspaceMonitor :: Maybe Text,
    hyprWorkspaceWindows :: Maybe Int
  }
  deriving (Show, Eq)

instance FromJSON HyprlandWorkspaceInfo where
  parseJSON = withObject "HyprlandWorkspaceInfo" $ \v ->
    HyprlandWorkspaceInfo
      <$> v .: "id"
      <*> v .: "name"
      <*> v .:? "monitor"
      <*> v .:? "windows"

-- | Entry from @hyprctl -j monitors@.
data HyprlandMonitorInfo = HyprlandMonitorInfo
  { hyprMonitorName :: Maybe Text,
    hyprMonitorId :: Maybe Int,
    hyprMonitorFocused :: Bool,
    hyprMonitorX :: Maybe Int,
    hyprMonitorY :: Maybe Int,
    hyprMonitorWidth :: Maybe Int,
    hyprMonitorHeight :: Maybe Int,
    hyprMonitorActiveWorkspace :: Maybe HyprlandWorkspaceRef
  }
  deriving (Show, Eq)

instance FromJSON HyprlandMonitorInfo where
  parseJSON = withObject "HyprlandMonitorInfo" $ \v ->
    HyprlandMonitorInfo
      <$> v .:? "name"
      <*> v .:? "id"
      <*> v .:? "focused" .!= False
      <*> v .:? "x"
      <*> v .:? "y"
      <*> v .:? "width"
      <*> v .:? "height"
      <*> v .:? "activeWorkspace"

-- | Entry from @hyprctl -j clients@.
data HyprlandClientInfo = HyprlandClientInfo
  { hyprClientAddress :: Text,
    hyprClientTitle :: Text,
    hyprClientInitialTitle :: Maybe Text,
    hyprClientClass :: Maybe Text,
    hyprClientInitialClass :: Maybe Text,
    hyprClientWorkspace :: HyprlandWorkspaceRef,
    hyprClientFocused :: Bool,
    hyprClientHidden :: Bool,
    hyprClientMapped :: Bool,
    hyprClientUrgent :: Bool,
    hyprClientAt :: Maybe (Int, Int)
  }
  deriving (Show, Eq)

instance FromJSON HyprlandClientInfo where
  parseJSON = withObject "HyprlandClientInfo" $ \v -> do
    at <- parseOptionalAt =<< (v .:? "at")
    HyprlandClientInfo
      <$> v .: "address"
      <*> v .:? "title" .!= ""
      <*> v .:? "initialTitle"
      <*> v .:? "class"
      <*> v .:? "initialClass"
      <*> v .: "workspace"
      <*> v .:? "focused" .!= False
      <*> v .:? "hidden" .!= False
      <*> v .:? "mapped" .!= True
      <*> v .:? "urgent" .!= False
      <*> pure at

-- | Result from @hyprctl -j activeworkspace@.
--
-- Hyprland has used multiple field spellings for the layout; we normalize those
-- into 'hawLayout'.
data HyprlandActiveWorkspaceInfo = HyprlandActiveWorkspaceInfo
  { hyprActiveWorkspaceId :: Maybe Int,
    hyprActiveWorkspaceName :: Maybe Text,
    hyprActiveWorkspaceLayout :: Maybe Text
  }
  deriving (Show, Eq)

instance FromJSON HyprlandActiveWorkspaceInfo where
  parseJSON = withObject "HyprlandActiveWorkspaceInfo" $ \v -> do
    layout <- v .:? "layout" <|> v .:? "layoutName" <|> v .:? "layoutname"
    HyprlandActiveWorkspaceInfo
      <$> v .:? "id"
      <*> v .:? "name"
      <*> pure layout

-- | Result from @hyprctl -j activewindow@.
data HyprlandActiveWindowInfo = HyprlandActiveWindowInfo
  { hyprActiveWindowAddress :: Text,
    hyprActiveWindowTitle :: Maybe Text,
    hyprActiveWindowClass :: Maybe Text
  }
  deriving (Show, Eq)

instance FromJSON HyprlandActiveWindowInfo where
  parseJSON = withObject "HyprlandActiveWindowInfo" $ \v ->
    HyprlandActiveWindowInfo
      <$> v .: "address"
      <*> v .:? "title"
      <*> v .:? "class"
