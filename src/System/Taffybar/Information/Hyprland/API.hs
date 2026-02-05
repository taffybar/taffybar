{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.Hyprland.API
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Typed, validated Hyprland operations built on top of
-- "System.Taffybar.Information.Hyprland".
--
-- This module is intended for widget use: it exposes typed query functions for
-- the common @hyprctl -j@ endpoints and a small set of typed dispatch commands.
-----------------------------------------------------------------------------

module System.Taffybar.Information.Hyprland.API
  ( -- * Queries
    getHyprlandClients
  , getHyprlandWorkspaces
  , getHyprlandMonitors
  , getHyprlandActiveWorkspace
  , getHyprlandActiveWindow

    -- * Dispatch
  , HyprlandWorkspaceTarget
  , mkHyprlandWorkspaceTarget
  , hyprlandWorkspaceTargetText
  , HyprlandAddress
  , mkHyprlandAddress
  , hyprlandAddressText
  , HyprlandDispatch(..)
  , dispatchHyprland
  ) where

import           Data.Aeson (FromJSON)
import qualified Data.ByteString as BS
import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T

import           System.Taffybar.Information.Hyprland
  ( HyprlandClient
  , HyprlandError(..)
  , hyprCommand
  , hyprCommandJson
  , runHyprlandCommandJson
  , runHyprlandCommandRaw
  )
import           System.Taffybar.Information.Hyprland.Types
  ( HyprlandActiveWindowInfo
  , HyprlandActiveWorkspaceInfo
  , HyprlandClientInfo
  , HyprlandMonitorInfo
  , HyprlandWorkspaceInfo
  )

runJson :: FromJSON a => HyprlandClient -> [String] -> IO (Either HyprlandError a)
runJson client args = runHyprlandCommandJson client (hyprCommandJson args)

getHyprlandClients :: HyprlandClient -> IO (Either HyprlandError [HyprlandClientInfo])
getHyprlandClients client = runJson client ["clients"]

getHyprlandWorkspaces :: HyprlandClient -> IO (Either HyprlandError [HyprlandWorkspaceInfo])
getHyprlandWorkspaces client = runJson client ["workspaces"]

getHyprlandMonitors :: HyprlandClient -> IO (Either HyprlandError [HyprlandMonitorInfo])
getHyprlandMonitors client = runJson client ["monitors"]

getHyprlandActiveWorkspace :: HyprlandClient -> IO (Either HyprlandError HyprlandActiveWorkspaceInfo)
getHyprlandActiveWorkspace client = runJson client ["activeworkspace"]

getHyprlandActiveWindow :: HyprlandClient -> IO (Either HyprlandError HyprlandActiveWindowInfo)
getHyprlandActiveWindow client = runJson client ["activewindow"]

newtype HyprlandWorkspaceTarget = HyprlandWorkspaceTarget Text
  deriving (Show, Eq)

hyprlandWorkspaceTargetText :: HyprlandWorkspaceTarget -> Text
hyprlandWorkspaceTargetText (HyprlandWorkspaceTarget t) = t

-- | Construct a workspace target validated enough to avoid socket argument splitting.
--
-- Note: This is stricter than @hyprctl@ (which can accept arguments with
-- whitespace), because the Hyprland command socket is a single string and we
-- currently build it with 'unwords'.
mkHyprlandWorkspaceTarget :: Text -> Either HyprlandError HyprlandWorkspaceTarget
mkHyprlandWorkspaceTarget t
  | T.null t = Left $ HyprlandCommandBuildFailed "Hyprland workspace target must not be empty"
  | T.any isSpace t = Left $ HyprlandCommandBuildFailed "Hyprland workspace target must not contain whitespace"
  | otherwise = Right $ HyprlandWorkspaceTarget t

newtype HyprlandAddress = HyprlandAddress Text
  deriving (Show, Eq)

hyprlandAddressText :: HyprlandAddress -> Text
hyprlandAddressText (HyprlandAddress t) = t

-- | Construct an address validated enough to prevent obvious argument splitting.
--
-- Hyprland currently represents window addresses like @0x123abc@, but we avoid
-- hard-coding that shape to reduce breakage if Hyprland changes formatting.
mkHyprlandAddress :: Text -> Either HyprlandError HyprlandAddress
mkHyprlandAddress t
  | T.null t = Left $ HyprlandCommandBuildFailed "Hyprland address must not be empty"
  | T.any isSpace t = Left $ HyprlandCommandBuildFailed "Hyprland address must not contain whitespace"
  | otherwise = Right $ HyprlandAddress t

data HyprlandDispatch
  = DispatchWorkspace HyprlandWorkspaceTarget
  -- ^ @hyprctl dispatch workspace <name-or-id>@
  | DispatchFocusWindowAddress HyprlandAddress
  -- ^ @hyprctl dispatch focuswindow address:<addr>@
  deriving (Show, Eq)

dispatchHyprland :: HyprlandClient -> HyprlandDispatch -> IO (Either HyprlandError BS.ByteString)
dispatchHyprland client action =
  runHyprlandCommandRaw client (hyprCommand (dispatchToArgs action))

dispatchToArgs :: HyprlandDispatch -> [String]
dispatchToArgs action =
  case action of
    DispatchWorkspace ws -> ["dispatch", "workspace", T.unpack (hyprlandWorkspaceTargetText ws)]
    DispatchFocusWindowAddress addr ->
      ["dispatch", "focuswindow", "address:" <> T.unpack (hyprlandAddressText addr)]
