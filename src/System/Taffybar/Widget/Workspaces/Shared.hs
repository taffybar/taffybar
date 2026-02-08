{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.Workspaces.Shared
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Shared UI helpers for workspace widgets (X11/EWMH and Hyprland).
--
-----------------------------------------------------------------------------

module System.Taffybar.Widget.Workspaces.Shared
  ( WorkspaceState(..)
  , getCSSClass
  , cssWorkspaceStates
  , setWorkspaceWidgetStatusClass
  , buildWorkspaceIconLabelOverlay
  , mkWorkspaceIconWidget
  ) where

import qualified Control.Concurrent.MVar as MV
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Int (Int32)
import qualified Data.Text as T

import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk

import           System.Taffybar.Widget.Generic.AutoSizeImage (autoSizeImage)
import           System.Taffybar.Widget.Util
  ( WindowIconWidget(..)
  , buildBottomLeftAlignedBox
  , buildContentsBox
  , buildOverlayWithPassThrough
  , mkWindowIconWidgetBase
  , updateWidgetClasses
  )

data WorkspaceState
  = Active
  | Visible
  | Hidden
  | Empty
  | Urgent
  deriving (Show, Eq)

getCSSClass :: (Show s) => s -> T.Text
getCSSClass = T.toLower . T.pack . show

cssWorkspaceStates :: [T.Text]
cssWorkspaceStates = map getCSSClass [Active, Visible, Hidden, Empty, Urgent]

setWorkspaceWidgetStatusClass ::
  (MonadIO m, Gtk.IsWidget a) => WorkspaceState -> a -> m ()
setWorkspaceWidgetStatusClass ws widget =
  updateWidgetClasses
    widget
    [getCSSClass ws]
    cssWorkspaceStates

-- | Build the common overlay layout used by workspace widgets:
-- window icons are the base content and the workspace label is overlaid in the
-- bottom-left corner.
buildWorkspaceIconLabelOverlay ::
  MonadIO m =>
  Gtk.Widget -> -- ^ Widget containing the window icon strip.
  Gtk.Widget -> -- ^ Workspace label widget.
  m Gtk.Widget
buildWorkspaceIconLabelOverlay iconsWidget labelWidget = do
  base <- buildContentsBox iconsWidget
  overlayLabel <- buildBottomLeftAlignedBox "overlay-box" labelWidget
  buildOverlayWithPassThrough base [overlayLabel]

-- | Build a 'WindowIconWidget' that automatically scales with allocation and
-- displays a transparent placeholder pixbuf when requested.
--
-- This is shared by both X11 and Hyprland workspace widgets so that CSS classes
-- and widget behavior remain consistent across backends.
mkWorkspaceIconWidget ::
  Maybe Int32 -> -- ^ Optional size request for the icon image.
  Bool -> -- ^ Whether to render a transparent placeholder when there is no data.
  (Int32 -> a -> IO (Maybe Gdk.Pixbuf)) -> -- ^ Icon pixbuf getter.
  (Int32 -> IO Gdk.Pixbuf) -> -- ^ Transparent placeholder pixbuf generator.
  IO (WindowIconWidget a)
mkWorkspaceIconWidget mSize transparentOnNone getPixbufFor mkTransparent = do
  base <- mkWindowIconWidgetBase mSize
  let getPixbuf size = do
        mWin <- MV.readMVar (iconWindow base)
        case mWin of
          Nothing ->
            if transparentOnNone
              then Just <$> mkTransparent size
              else return Nothing
          Just w -> do
            pb <- getPixbufFor size w
            case pb of
              Just _ -> return pb
              Nothing ->
                if transparentOnNone
                  then Just <$> mkTransparent size
                  else return Nothing
  refreshImage <- autoSizeImage (iconImage base) getPixbuf Gtk.OrientationHorizontal
  return base { iconForceUpdate = refreshImage }

