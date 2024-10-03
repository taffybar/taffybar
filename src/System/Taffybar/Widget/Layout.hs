{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.Layout
-- Copyright   : (c) Ivan Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple text widget that shows the XMonad layout used in the currently active
-- workspace, and that allows to change it by clicking with the mouse:
-- left-click to switch to the next layout in the list, right-click to switch to
-- the first one (as configured in @xmonad.hs@)
-----------------------------------------------------------------------------

module System.Taffybar.Widget.Layout
  (
  -- * Usage
  -- $usage
    LayoutConfig(..)
  , defaultLayoutConfig
  , layoutNew
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Default (Default(..))
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import           GI.Gdk
import           System.Taffybar.Context
import           System.Taffybar.Information.X11DesktopInfo
import           System.Taffybar.Util
import           System.Taffybar.Widget.Util

-- $usage
--
-- This widget requires that the "XMonad.Hooks.TaffybarPagerHints" hook be
-- installed in your @xmonad.hs@:
--
-- > import XMonad.Hooks.TaffybarPagerHints (pagerHints)
-- > main = do
-- >   xmonad $ ewmh $ pagerHints $ defaultConfig
-- > ...
--
-- Once you've properly configured @xmonad.hs@, you can use the widget in
-- your @taffybar.hs@ file:
--
-- > import System.Taffybar.Widget.Layout
-- > main = do
-- >   let los = layoutSwitcherNew def
--
-- now you can use @los@ as any other Taffybar widget.

newtype LayoutConfig = LayoutConfig
  { formatLayout :: T.Text -> TaffyIO T.Text
  }

defaultLayoutConfig :: LayoutConfig
defaultLayoutConfig = LayoutConfig return

instance Default LayoutConfig where
  def = defaultLayoutConfig

-- | Name of the X11 events to subscribe, and of the hint to look for for
-- the name of the current layout.
xLayoutProp :: String
xLayoutProp = "_XMONAD_CURRENT_LAYOUT"

-- | Create a new Layout widget that will use the given Pager as
-- its source of events.
layoutNew :: LayoutConfig -> TaffyIO Gtk.Widget
layoutNew config = do
  ctx <- ask
  label <- lift $ Gtk.labelNew (Nothing :: Maybe T.Text)
  _ <- widgetSetClassGI label "layout-label"

  -- This callback is run in a separate thread and needs to use
  -- postGUIASync
  let callback _ = mapReaderT postGUIASync $ do
        layout <- runX11Def "" $ readAsString Nothing xLayoutProp
        markup <- formatLayout config (T.pack layout)
        lift $ Gtk.labelSetMarkup label markup

  subscription <- subscribeToPropertyEvents [xLayoutProp] callback

  do
    ebox <- Gtk.eventBoxNew
    Gtk.containerAdd ebox label
    _ <- Gtk.onWidgetButtonPressEvent ebox $ dispatchButtonEvent ctx
    Gtk.widgetShowAll ebox
    _ <- Gtk.onWidgetUnrealize ebox $ runTaffy ctx $ unsubscribe subscription
    Gtk.toWidget ebox

-- | Call 'switch' with the appropriate argument (1 for left click, -1 for
-- right click), depending on the click event received.
dispatchButtonEvent :: Context -> EventButton -> IO Bool
dispatchButtonEvent context btn = do
  ev <- isLR <$> getEventButtonType btn <*> getEventButtonButton btn
  case ev of
    Just inc -> do
      runTaffy context (runX11Def () (switch inc))
      return True
    Nothing -> return False

  where
    isLR EventTypeButtonPress buttonNumber = case buttonNumber of
      1 -> Just 1
      2 -> Just (-1)
      _ -> Nothing
    isLR _ _ = Nothing

-- | Emit a new custom event of type _XMONAD_CURRENT_LAYOUT, that can be
-- intercepted by the PagerHints hook, which in turn can instruct XMonad to
-- switch to a different layout.
switch :: Int -> X11Property ()
switch n = do
  cmd <- getAtom xLayoutProp
  sendCommandEvent cmd (fromIntegral n)
