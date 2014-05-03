-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widgets.Util
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Utility functions to facilitate building GTK interfaces.
--
-----------------------------------------------------------------------------

module System.Taffybar.Widgets.Util where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Gtk

-- | Execute the given action as a response to any of the given types
-- of mouse button clicks.
onClick :: [Click] -- ^ Types of button clicks to listen to.
        -> IO a    -- ^ Action to execute.
        -> EventM EButton Bool
onClick triggers action = tryEvent $ do
  click <- eventClick
  when (click `elem` triggers) $ liftIO action >> return ()

-- | Attach the given widget as a popup with the given title to the
-- given window. The newly attached popup is not shown initially. Use
-- the 'displayPopup' function to display it.
attachPopup :: (WidgetClass w, WindowClass wnd) =>
               w      -- ^ The widget to set as popup.
            -> String -- ^ The title of the popup.
            -> wnd    -- ^ The window to attach the popup to.
            -> IO ()
attachPopup widget title window = do
  set window [ windowTitle := title
             , windowTypeHint := WindowTypeHintTooltip
             , windowSkipTaskbarHint := True
             ]
  windowSetSkipPagerHint window True
  windowSetKeepAbove window True
  windowStick window
  Just topLevel <- widgetGetAncestor widget gTypeWindow
  let topLevelWindow = castToWindow topLevel
  windowSetTransientFor window topLevelWindow

-- | Display the given popup widget (previously prepared using the
-- 'attachPopup' function) immediately beneath (or above) the given
-- window.
displayPopup :: (WidgetClass w, WindowClass wnd) =>
                w   -- ^ The popup widget.
             -> wnd -- ^ The window the widget was attached to.
             -> IO ()
displayPopup widget window = do
  windowSetPosition window WinPosMouse
  (x, y ) <- windowGetPosition window
  (_, y') <- widgetGetSize widget
  widgetShowAll window
  if y > y'
    then windowMove window x (y - y')
    else windowMove window x y'
