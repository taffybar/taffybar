{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      : StatusNotifier.TransparentWindow
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
module StatusNotifier.TransparentWindow where

import           Control.Monad.IO.Class
import           GI.Cairo.Render
import           GI.Cairo.Render.Connector
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

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
  flip renderWithContext context $ do
    setOperator OperatorSource
    paint
    setOperator OperatorOver
  return False
