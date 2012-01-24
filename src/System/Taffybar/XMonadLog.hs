{-# LANGUAGE OverloadedStrings #-}
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
module System.Taffybar.XMonadLog ( xmonadLogNew, dbusLog, dbusLogWithPP, taffybarPP, taffybarColor, taffybarEscape ) where

import Codec.Binary.UTF8.String ( decodeString )
import DBus.Client.Simple ( connectSession, emit, Client )
import DBus.Client ( listen, MatchRule(..) )
import DBus.Types
import DBus.Message
import Graphics.UI.Gtk hiding ( Signal )

import XMonad
import XMonad.Hooks.DynamicLog

import Web.Encodings ( decodeHtml, encodeHtml )

-- | This is a DBus-based logger that can be used from XMonad to log
-- to this widget.

dbusLogWithPP :: Client -> PP -> X ()
dbusLogWithPP client pp = dynamicLogWithPP pp { ppOutput = outputThroughDBus client }

dbusLog :: Client -> X ()
dbusLog client = dbusLogWithPP client taffybarDefaultPP

taffybarColor :: String -> String -> String -> String
taffybarColor fg bg = wrap t "</span>" . taffybarEscape
  where t = concat ["<span fgcolor=\"", fg, if null bg then "" else "\" bgcolor=\"" ++ bg , "\">"]

taffybarEscape :: String -> String
taffybarEscape = encodeHtml . decodeHtml

-- same as defaultPP
taffybarDefaultPP :: PP
taffybarDefaultPP = defaultPP { ppCurrent         = taffybarEscape . wrap "[" "]"
                              , ppVisible         = taffybarEscape . wrap "<" ">"
			      , ppHidden          = taffybarEscape
			      , ppHiddenNoWindows = taffybarEscape
			      , ppUrgent          = taffybarEscape
                              , ppTitle           = taffybarEscape . shorten 80
			      , ppLayout          = taffybarEscape
			      }
-- Same as xmobarPP
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
  emit client "/org/xmonad/Log" "org.xmonad.Log" "Update" [ toVariant str' ]

setupDbus :: Label -> IO ()
setupDbus w = do
  let matcher = MatchRule { matchSender = Nothing
                          , matchDestination = Nothing
                          , matchPath = Just "/org/xmonad/Log"
                          , matchInterface = Just "org.xmonad.Log"
                          , matchMember = Just "Update"
                          }

  client <- connectSession

  listen client matcher (callback w)

callback :: Label -> BusName -> Signal -> IO ()
callback w _ sig = do
  let [bdy] = signalBody sig
      Just status = fromVariant bdy
  postGUIAsync $ labelSetMarkup w status

xmonadLogNew :: IO Widget
xmonadLogNew = do
  l <- labelNew Nothing
  _ <- on l realize $ setupDbus l
  widgetShowAll l
  return (toWidget l)
