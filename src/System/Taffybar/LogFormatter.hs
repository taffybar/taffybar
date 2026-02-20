-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.LogFormatter
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- ANSI-colored log formatting helpers for taffybar logging output.
module System.Taffybar.LogFormatter where

import System.Console.ANSI
import System.IO
import System.Log.Formatter
import System.Log.Handler.Simple
import System.Log.Logger
import Text.Printf

-- | Build an ANSI foreground color escape sequence.
setColor :: Color -> String
setColor color = setSGRCode [SetColor Foreground Vivid color]

-- | Choose a color for a log priority.
priorityToColor :: Priority -> Color
priorityToColor CRITICAL = Red
priorityToColor ALERT = Red
priorityToColor EMERGENCY = Red
priorityToColor ERROR = Red
priorityToColor WARNING = Yellow
priorityToColor NOTICE = Magenta
priorityToColor INFO = Blue
priorityToColor DEBUG = Green

-- | ANSI reset sequence.
reset :: String
reset = setSGRCode [Reset]

-- | Wrap text in ANSI color escapes and reset formatting afterward.
colorize :: Color -> String -> String
colorize color txt = setColor color <> txt <> reset

-- | Formatter used for taffybar logs.
taffyLogFormatter :: LogFormatter a
taffyLogFormatter _ (level, msg) name =
  return $ printf "%s %s - %s" colorizedPriority colorizedName msg
  where
    priorityColor = priorityToColor level
    colorizedPriority =
      colorize
        priorityColor
        ("[" <> show level <> "]")
    colorizedName = colorize Green name

-- | Default stderr log handler configured with 'taffyLogFormatter'.
taffyLogHandler :: IO (GenericHandler Handle)
taffyLogHandler = setFormatter <$> streamHandler stderr DEBUG
  where
    setFormatter h = h {formatter = taffyLogFormatter}
