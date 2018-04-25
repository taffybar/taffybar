-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Layout
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple text widget that shows the XMonad layout used in the currently
-- active workspace, and that allows to change it by clicking with the
-- mouse: left-click to switch to the next layout in the list, right-click
-- to switch to the first one (as configured in @xmonad.hs@)
-----------------------------------------------------------------------------

module System.Taffybar.Layout (
  -- * Usage
  -- $usage
    LayoutConfig(..)
  , defaultLayoutConfig
  , layoutNew
) where

import Control.Monad.Trans
import Control.Monad.Reader
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Abstract.Widget as W
import System.Information.X11DesktopInfo
import System.Taffybar.Widgets.Util
import System.Taffybar.Context

-- $usage
--
-- This widget requires that the "System.Taffybar.Hooks.PagerHints" hook be
-- installed in your @xmonad.hs@:
--
-- > import System.Taffybar.Hooks.PagerHints (pagerHints)
-- > main = do
-- >   xmonad $ ewmh $ pagerHints $ defaultConfig
-- > ...
--
-- Once you've properly configured @xmonad.hs@, you can use the widget in
-- your @taffybar.hs@ file:
--
-- > import System.Taffybar.Layout
-- > main = do
-- >   let los = layoutSwitcherNew defaultLayoutConfig
--
-- now you can use @los@ as any other Taffybar widget.

data LayoutConfig = LayoutConfig
  { formatLayout :: String -> TaffyIO String
  }

defaultLayoutConfig :: LayoutConfig
defaultLayoutConfig = LayoutConfig return

-- | Name of the X11 events to subscribe, and of the hint to look for for
-- the name of the current layout.
xLayoutProp :: String
xLayoutProp = "_XMONAD_CURRENT_LAYOUT"

-- | Create a new Layout widget that will use the given Pager as
-- its source of events.
layoutNew :: LayoutConfig -> TaffyIO Gtk.Widget
layoutNew config = do
  ctx <- ask
  label <- lift $ Gtk.labelNew (Nothing :: Maybe String)

  -- This callback is run in a separate thread and needs to use
  -- postGUIAsync
  let callback _ = liftReader Gtk.postGUIAsync $ do
        layout <- runX11Def "" $ readAsString Nothing xLayoutProp
        markup <- formatLayout config layout
        lift $ Gtk.labelSetMarkup label markup

  subscription <- subscribeToEvents [xLayoutProp] callback

  lift $ do
    ebox <- Gtk.eventBoxNew
    Gtk.containerAdd ebox label
    _ <- Gtk.on ebox Gtk.buttonPressEvent $ dispatchButtonEvent ctx
    Gtk.widgetShowAll ebox
    _ <- Gtk.on ebox W.unrealize $ flip runReaderT ctx $ unsubscribe subscription
    return $ Gtk.toWidget ebox

-- | Call 'switch' with the appropriate argument (1 for left click, -1 for
-- right click), depending on the click event received.
dispatchButtonEvent :: Context -> Gtk.EventM Gtk.EButton Bool
dispatchButtonEvent context = do
  btn <- Gtk.eventButton
  let trigger prop =
        onClick [Gtk.SingleClick] $
                runReaderT (runX11Def () prop) context >> return True
  case btn of
    Gtk.LeftButton  -> trigger $ switch 1
    Gtk.RightButton -> trigger $ switch (-1)
    _               -> return False

-- | Emit a new custom event of type _XMONAD_CURRENT_LAYOUT, that can be
-- intercepted by the PagerHints hook, which in turn can instruct XMonad to
-- switch to a different layout.
switch :: Int -> X11Property ()
switch n = do
  cmd <- getAtom xLayoutProp
  sendCommandEvent cmd (fromIntegral n)
