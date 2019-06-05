-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.LogFormatter
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------
module System.Taffybar.LogFormatter where

import System.Console.ANSI
import System.Log.Formatter
import System.Log.Handler.Simple
import System.Log.Logger
import Text.Printf
import System.IO

setColor :: Color -> String
setColor color = setSGRCode [SetColor Foreground Vivid color]

priorityToColor :: Priority -> Color
priorityToColor CRITICAL = Red
priorityToColor ALERT = Red
priorityToColor EMERGENCY = Red
priorityToColor ERROR = Red
priorityToColor WARNING = Yellow
priorityToColor NOTICE = Magenta
priorityToColor INFO = Blue
priorityToColor DEBUG = Green

reset :: String
reset = setSGRCode [Reset]

colorize color txt = setColor color <> txt <> reset

taffyLogFormatter :: LogFormatter a
taffyLogFormatter _ (priority, msg) name =
  return $ printf "%s %s - %s" colorizedPriority colorizedName msg
    where priorityColor = priorityToColor priority
          colorizedPriority = colorize priorityColor
                              ("[" <> show priority <> "]")
          colorizedName = colorize Green name

taffyLogHandler = setFormatter <$> streamHandler stderr DEBUG
  where setFormatter h = h { formatter = taffyLogFormatter }
