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
module System.Taffybar.XMonadLog ( xmonadLogNew, dbusLog ) where

import DBus.Client.Simple ( connectSession, emit, Client )
import DBus.Client ( listen, MatchRule(..) )
import DBus.Types
import DBus.Message
import Graphics.UI.Gtk hiding ( Signal )

import XMonad
import XMonad.Hooks.DynamicLog

-- | This is a DBus-based logger that can be used from XMonad to log
-- to this widget.
dbusLog :: Client -> PP -> X ()
dbusLog client pp = do
  dynamicLogWithPP pp { ppOutput = outputThroughDBus client }

outputThroughDBus :: Client -> String -> IO ()
outputThroughDBus client str =
  emit client "/org/xmonad/Log" "org.xmonad.Log" "Update" [ toVariant str ]

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
