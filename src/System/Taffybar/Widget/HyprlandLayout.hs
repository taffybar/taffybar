{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.HyprlandLayout
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple text widget that shows the Hyprland layout used in the currently
-- active workspace.
-----------------------------------------------------------------------------

module System.Taffybar.Widget.HyprlandLayout
  ( HyprlandLayoutConfig(..)
  , defaultHyprlandLayoutConfig
  , hyprlandLayoutNew
  ) where

import           Control.Applicative ((<|>))
import           Control.Concurrent (killThread)
import           Control.Monad (void)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson (FromJSON(..), withObject, (.:?))
import           Data.Default (Default(..))
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           GI.Gdk
import qualified GI.Gtk as Gtk
import           System.Log.Logger (Priority(..))
import           System.Taffybar.Context
import           System.Taffybar.Hyprland
  ( runHyprlandCommandJsonT
  , runHyprlandCommandRawT
  )
import qualified System.Taffybar.Information.Hyprland as Hypr
import           System.Taffybar.Util
import           System.Taffybar.Widget.Util

data HyprlandLayoutConfig = HyprlandLayoutConfig
  { formatLayout :: T.Text -> TaffyIO T.Text
  , updateIntervalSeconds :: Double
  , onLeftClick :: Maybe [String]
  , onRightClick :: Maybe [String]
  }

instance Default HyprlandLayoutConfig where
  def = defaultHyprlandLayoutConfig

defaultHyprlandLayoutConfig :: HyprlandLayoutConfig
defaultHyprlandLayoutConfig =
  HyprlandLayoutConfig
  { formatLayout = return
  , updateIntervalSeconds = 1
  , onLeftClick = Nothing
  , onRightClick = Nothing
  }

-- | Create a new Hyprland Layout widget.
hyprlandLayoutNew :: HyprlandLayoutConfig -> TaffyIO Gtk.Widget
hyprlandLayoutNew config = do
  ctx <- ask
  label <- lift $ Gtk.labelNew (Nothing :: Maybe T.Text)
  _ <- widgetSetClassGI label "layout-label"

  let refresh = do
        layoutText <- getHyprlandLayoutText
        markup <- formatLayout config layoutText
        lift $ postGUIASync $ Gtk.labelSetMarkup label markup

  void refresh
  threadId <- lift $ foreverWithDelay (updateIntervalSeconds config) $
    void $ runReaderT refresh ctx

  ebox <- lift Gtk.eventBoxNew
  lift $ Gtk.containerAdd ebox label
  _ <- lift $ Gtk.onWidgetButtonPressEvent ebox $ dispatchButtonEvent ctx config
  _ <- lift $ Gtk.onWidgetUnrealize ebox $ killThread threadId
  lift $ Gtk.widgetShowAll ebox
  Gtk.toWidget ebox

-- | Call the configured dispatch action depending on click.
dispatchButtonEvent :: Context -> HyprlandLayoutConfig -> EventButton -> IO Bool
dispatchButtonEvent context config btn = do
  pressType <- getEventButtonType btn
  buttonNumber <- getEventButtonButton btn
  case pressType of
    EventTypeButtonPress ->
      case buttonNumber of
        1 -> runReaderT (dispatchMaybe $ onLeftClick config) context >> return True
        3 -> runReaderT (dispatchMaybe $ onRightClick config) context >> return True
        _ -> return False
    _ -> return False

-- | Dispatch a Hyprland command if provided.
dispatchMaybe :: Maybe [String] -> TaffyIO ()
dispatchMaybe maybeArgs =
  case maybeArgs of
    Nothing -> return ()
    Just args -> do
      result <- runHyprlandCommandRawT (Hypr.hyprCommand ("dispatch" : args))
      case result of
        Left err ->
          logPrintF "System.Taffybar.Widget.HyprlandLayout" WARNING
            "Failed to dispatch Hyprland command: %s" (show err)
        Right _ -> return ()

-- Hyprland JSON helpers

newtype HyprlandActiveWorkspace = HyprlandActiveWorkspace
  { hawLayout :: Maybe Text
  } deriving (Show, Eq)

instance FromJSON HyprlandActiveWorkspace where
  parseJSON = withObject "HyprlandActiveWorkspace" $ \v -> do
    layout <- v .:? "layout" <|> v .:? "layoutName" <|> v .:? "layoutname"
    return $ HyprlandActiveWorkspace layout

getHyprlandLayoutText :: TaffyIO T.Text
getHyprlandLayoutText = do
  result <- runHyprctlJson ["-j", "activeworkspace"]
  case result of
    Left err ->
      logPrintF "System.Taffybar.Widget.HyprlandLayout" WARNING
        "hyprctl activeworkspace failed: %s" err >>
      return ""
    Right (HyprlandActiveWorkspace layout) ->
      return $ fromMaybe "" layout

runHyprctlJson :: FromJSON a => [String] -> TaffyIO (Either String a)
runHyprctlJson args = do
  let args' =
        case args of
          ("-j":rest) -> rest
          _ -> args
  result <- runHyprlandCommandJsonT (Hypr.hyprCommandJson args')
  pure $ case result of
    Left err -> Left (show err)
    Right out -> Right out
