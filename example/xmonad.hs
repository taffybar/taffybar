module Main where

import           Data.Default                    (def)
import           XMonad
import           XMonad.Hooks.EwmhDesktops       (ewmh)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.TaffybarPagerHints (pagerHints)

main = xmonad $
       -- docks allows xmonad to handle taffybar
       docks $
       -- ewmh allows taffybar access to the state of xmonad/x11
       ewmh $
       -- pagerHints supplies additional state that is not supplied by ewmh
       pagerHints
       -- Apply these to the default XMonad config
       def
