{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This widget listens on DBus for Log events from XMonad and
-- displays the formatted status string.  To log to this widget using
-- the excellent dbus-core library, use code like the following:
--
-- > import DBus.Client.Simple
-- > main = do
-- >   session <- connectSession
-- >   emit session "/org/xmonad/Log" "org.xmonad.Log" "Update" [toVariant "msg"]
--
-- There is a more complete example of xmonad integration in the
-- top-level module.
module System.Taffybar.XMonadLog {-# DEPRECATED "Use TaffyPager instead.  This module will be removed." #-} (
  -- * Constructor
  xmonadLogNew,
  -- * Log hooks for xmonad.hs
  dbusLog,
  dbusLogWithPP,
  -- * Styles
  taffybarPP,
  taffybarDefaultPP,
  taffybarColor,
  taffybarEscape
  ) where

import Codec.Binary.UTF8.String ( decodeString )
import Control.Monad ( void )
import DBus ( toVariant, fromVariant, Signal(..), signal )
import DBus.Client ( addMatch, matchAny, MatchRule(..), connectSession, emit, Client )
import Graphics.UI.Gtk hiding ( Signal )

import XMonad
import XMonad.Hooks.DynamicLog

-- | This is a DBus-based logger that can be used from XMonad to log
-- to this widget.  This version lets you specify the format for the
-- log using a pretty printer (e.g., 'taffybarPP').
dbusLogWithPP :: Client -> PP -> X ()
dbusLogWithPP client pp = dynamicLogWithPP pp { ppOutput = outputThroughDBus client }

-- | A DBus-based logger with a default pretty-print configuration
dbusLog :: Client -> X ()
dbusLog client = dbusLogWithPP client taffybarDefaultPP

taffybarColor :: String -> String -> String -> String
taffybarColor fg bg = wrap t "</span>" . taffybarEscape
  where
    t = concat ["<span fgcolor=\"", fg, if null bg then "" else "\" bgcolor=\"" ++ bg , "\">"]

-- | Escape strings so that they can be safely displayed by Pango in
-- the bar widget
taffybarEscape :: String -> String
taffybarEscape = escapeMarkup

-- | The same as the default PP in XMonad.Hooks.DynamicLog
taffybarDefaultPP :: PP
taffybarDefaultPP =
#if MIN_VERSION_xmonad_contrib(0, 12, 0)
  def {
#else
  defaultPP {
#endif
    ppCurrent         = taffybarEscape . wrap "[" "]"
    , ppVisible         = taffybarEscape . wrap "<" ">"
    , ppHidden          = taffybarEscape
    , ppHiddenNoWindows = taffybarEscape
    , ppUrgent          = taffybarEscape
    , ppTitle           = taffybarEscape . shorten 80
    , ppLayout          = taffybarEscape
    }
-- | The same as xmobarPP in XMonad.Hooks.DynamicLog
taffybarPP :: PP
taffybarPP = taffybarDefaultPP { ppCurrent = taffybarColor "yellow" "" . wrap "[" "]"
                               , ppTitle   = taffybarColor "green"  "" . shorten 40
                               , ppVisible = wrap "(" ")"
                               , ppUrgent  = taffybarColor "red" "yellow"
                               }



outputThroughDBus :: Client -> String -> IO ()
outputThroughDBus client str = do
  -- The string that we get from XMonad here isn't quite a normal
  -- string - each character is actually a byte in a utf8 encoding.
  -- We need to decode the string back into a real String before we
  -- send it over dbus.
  let str' = decodeString str
  emit client (signal "/org/xmonad/Log" "org.xmonad.Log" "Update") { signalBody = [ toVariant str' ] }

setupDbus :: Label -> IO ()
setupDbus w = do
  let matcher = matchAny { matchSender = Nothing
                          , matchDestination = Nothing
                          , matchPath = Just "/org/xmonad/Log"
                          , matchInterface = Just "org.xmonad.Log"
                          , matchMember = Just "Update"
                          }

  client <- connectSession

  void $ addMatch client matcher (callback w)

callback :: Label -> Signal -> IO ()
callback w sig = do
  let [bdy] = signalBody sig
      status :: String
      Just status = fromVariant bdy
  postGUIAsync $ labelSetMarkup w status

-- | Return a new XMonad log widget
xmonadLogNew :: IO Widget
xmonadLogNew = do
  l <- labelNew (Nothing :: Maybe String)
  _ <- on l realize $ setupDbus l
  widgetShowAll l
  return (toWidget l)

{-# DEPRECATED xmonadLogNew "Use taffyPagerNew instead." #-}
