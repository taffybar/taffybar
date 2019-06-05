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
import Data.Monoid

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

colorize :: Color -> String -> String
colorize color txt = setColor color <> txt <> reset

taffyLogFormatter :: LogFormatter a
taffyLogFormatter _ (level, msg) name =
  return $ printf "%s %s - %s" colorizedPriority colorizedName msg
    where priorityColor = priorityToColor level
          colorizedPriority = colorize priorityColor
                              ("[" <> show level <> "]")
          colorizedName = colorize Green name

taffyLogHandler :: IO (GenericHandler Handle)
taffyLogHandler = setFormatter <$> streamHandler stderr DEBUG
  where setFormatter h = h { formatter = taffyLogFormatter }
