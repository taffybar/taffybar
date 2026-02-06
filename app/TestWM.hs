module Main (main) where

import XMonad (def, manageHook, xmonad)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (docks)
import XMonad.ManageHook
  ( (-->)
  , (<+>)
  , className
  , composeAll
  , doShift
  , (=?)
  )

-- Minimal EWMH-compliant WM used by CI appearance tests.
--
-- This is a normal executable so the tests can start a WM without invoking
-- xmonad's usual runtime recompilation of ~/.xmonad/xmonad.hs.
main :: IO ()
main =
  xmonad $
    ewmhFullscreen $
      ewmh $
        docks $
          def
            { manageHook =
                composeAll
                  [ className =? "GreenWin" --> doShift "2"
                  ] <+> manageHook def
            }
