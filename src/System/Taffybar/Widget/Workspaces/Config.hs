{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Workspaces.Config
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Shared configuration shape used by both workspace widgets.
module System.Taffybar.Widget.Workspaces.Config
  ( WorkspaceWidgetCommonConfig (..),
  )
where

import Data.Int (Int32)
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import System.Taffybar.Context (TaffyIO)

-- | Common, backend-agnostic workspace-widget configuration.
--
-- The @m@ type parameter captures the backend-specific context monad used for
-- widget/controller updates.
data WorkspaceWidgetCommonConfig m workspace window controller
  = WorkspaceWidgetCommonConfig
  { widgetBuilder :: workspace -> m controller,
    widgetGap :: Int,
    maxIcons :: Maybe Int,
    minIcons :: Int,
    getWindowIconPixbuf :: Int32 -> window -> TaffyIO (Maybe Gdk.Pixbuf),
    labelSetter :: workspace -> m String,
    showWorkspaceFn :: workspace -> Bool,
    iconSort :: [window] -> m [window],
    urgentWorkspaceState :: Bool
  }
