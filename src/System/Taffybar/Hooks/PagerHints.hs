-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Hooks.PagerHints
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Complements the "XMonad.Hooks.EwmhDesktops" with two additional hints
-- not contemplated by the EWMH standard:
--
-- [@_XMONAD_CURRENT_LAYOUT@] Contains a UTF-8 string with the name of the
-- windows layout currently used in the active workspace.
--
-- [@_XMONAD_VISIBLE_WORKSPACES@] Contains a list of UTF-8 strings with the
-- names of all the workspaces that are currently showed in a secondary
-- display, or an empty list if in the current installation there's only
-- one monitor.
--
-- The first hint can be set directly on the root window of the default
-- display, or indirectly via X11 events with an atom of the same
-- name. This allows both to track any changes that occur in the layout of
-- the current workspace, as well as to have it changed automatically by
-- just sending a custom event to the hook.
--
-- The second one should be considered read-only, and is set every time
-- XMonad calls its log hooks.
--
-----------------------------------------------------------------------------

module System.Taffybar.Hooks.PagerHints (
  -- * Usage
  -- $usage
  pagerHints
) where

import Codec.Binary.UTF8.String (encode)
import Control.Monad
import Data.Monoid
import Foreign.C.Types (CInt)
import XMonad
import qualified XMonad.StackSet as W

-- $usage
--
-- You can use this module with the following in your @xmonad.hs@ file:
--
-- > import System.Taffybar.Hooks.PagerHints (pagerHints)
-- >
-- > main = xmonad $ ewmh $ pagerHints $ defaultConfig
-- > ...

-- | The \"Current Layout\" custom hint.
xLayoutProp :: X Atom
xLayoutProp = return =<< getAtom "_XMONAD_CURRENT_LAYOUT"

-- | The \"Visible Workspaces\" custom hint.
xVisibleProp :: X Atom
xVisibleProp = return =<< getAtom "_XMONAD_VISIBLE_WORKSPACES"

-- | Add support for the \"Current Layout\" and \"Visible Workspaces\" custom
-- hints to the given config.
pagerHints :: XConfig a -> XConfig a
pagerHints c = c { handleEventHook = handleEventHook c +++ pagerHintsEventHook
           , logHook = logHook c +++ pagerHintsLogHook }
  where x +++ y = x `mappend` y

-- | Update the current values of both custom hints.
pagerHintsLogHook :: X ()
pagerHintsLogHook = do
  withWindowSet
    (setCurrentLayout . description . W.layout . W.workspace . W.current)
  withWindowSet
    (setVisibleWorkspaces . map (W.tag . W.workspace) . W.visible)

-- | Set the value of the \"Current Layout\" custom hint to the one given.
setCurrentLayout :: String -> X ()
setCurrentLayout l = withDisplay $ \dpy -> do
  r <- asks theRoot
  a <- xLayoutProp
  c <- getAtom "UTF8_STRING"
  let l' = map fromIntegral (encode l)
  io $ changeProperty8 dpy r a c propModeReplace l'

-- | Set the value of the \"Visible Workspaces\" hint to the one given.
setVisibleWorkspaces :: [String] -> X ()
setVisibleWorkspaces vis = withDisplay $ \dpy -> do
  r  <- asks theRoot
  a  <- xVisibleProp
  c  <- getAtom "UTF8_STRING"
  let vis' = map fromIntegral $ concatMap ((++[0]) . encode) vis
  io $ changeProperty8 dpy r a c propModeReplace vis'

-- | Handle all \"Current Layout\" events received from pager widgets, and
-- set the current layout accordingly.
pagerHintsEventHook :: Event -> X All
pagerHintsEventHook (ClientMessageEvent {
    ev_message_type = mt,
    ev_data = d
  }) = withWindowSet $ \_ -> do
  a <- xLayoutProp
  when (mt == a) $ sendLayoutMessage d
  return (All True)
pagerHintsEventHook _ = return (All True)

-- | Request a change in the current layout by sending an internal message
-- to XMonad.
sendLayoutMessage :: [CInt] -> X ()
sendLayoutMessage evData = case evData of
  []   -> return ()
  x:_  -> if x < 0
            then sendMessage FirstLayout
            else sendMessage NextLayout
