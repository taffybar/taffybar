{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.TransparentWindow
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Make a window transparent. Approach adapted from python code from
-- https://stackoverflow.com/questions/3908565/how-to-make-gtk-window-background-transparent/33294727#33294727
-----------------------------------------------------------------------------
module System.Taffybar.TransparentWindow where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.GI.Base
import           Foreign.Ptr (castPtr)
import qualified GI.Cairo
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import           Graphics.Rendering.Cairo
import           Graphics.Rendering.Cairo.Internal (Render(runRender))
import           Graphics.Rendering.Cairo.Types (Cairo(Cairo))

-- | This function bridges gi-cairo with the hand-written cairo package. It
-- takes a `GI.Cairo.Context` (as it appears in gi-cairo), and a `Render` action
-- (as in the cairo lib), and renders the `Render` action into the given
-- context.
renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r =
  withManagedPtr ct $ \p -> runReaderT (runRender r) (Cairo (castPtr p))

makeWindowTransparent :: MonadIO m => Gtk.Window -> m ()
makeWindowTransparent window = do
  screen <- Gtk.widgetGetScreen window
  visual <- Gdk.screenGetRgbaVisual screen
  Gtk.widgetSetVisual window visual
  Gtk.setWidgetAppPaintable window True
  _ <- Gtk.onWidgetDraw window transparentDraw
  return ()

transparentDraw :: Gtk.WidgetDrawCallback
transparentDraw context = do
  rGBA <- Gdk.newZeroRGBA
  Gdk.setRGBAAlpha rGBA 0.0
  Gdk.setRGBABlue rGBA 1.0
  Gdk.setRGBARed rGBA 1.0
  Gdk.setRGBAGreen rGBA 1.0
  Gdk.cairoSetSourceRgba context rGBA
  renderWithContext context $ do
    setOperator OperatorSource
    paint
    setOperator OperatorOver
  return False
