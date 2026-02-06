module Main (main) where

import XMonad (def, xmonad)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (docks)

-- Minimal EWMH-compliant WM used by CI appearance tests.
--
-- This is a normal executable so the tests can start a WM without invoking
-- xmonad's usual runtime recompilation of ~/.xmonad/xmonad.hs.
main :: IO ()
main = xmonad $ ewmhFullscreen $ ewmh $ docks def

