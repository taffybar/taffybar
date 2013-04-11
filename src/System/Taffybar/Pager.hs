-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Pager
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Common support for pager widgets. This module does not provide itself
-- any widgets, but implements an event dispatcher on which widgets can
-- subscribe the desktop events they're interested in, as well as common
-- configuration facilities.
--
-- N.B. If you're just looking for a drop-in replacement for the
-- "System.Taffybar.XMonadLog" widget that is clickable and doesn't require
-- DBus, you may want to see first "System.Taffybar.TaffyPager".
--
-- You need only one Pager component to instantiate any number of pager
-- widgets:
--
-- > pager <- pagerNew defaultPagerConfig
-- >
-- > let wss = wspaceSwitcherNew pager  -- Workspace Switcher widget
-- >     los = layoutSwitcherNew pager  -- Layout Switcher widget
-- >     wnd = windowSwitcherNew pager  -- Window Switcher widget
--
-----------------------------------------------------------------------------

module System.Taffybar.Pager
  ( Pager (config)
  , PagerConfig (..)
  , defaultPagerConfig
  , pagerNew
  , subscribe
  , colorize
  , shorten
  , wrap
  , escape
  ) where

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad.Reader
import Data.IORef
import Graphics.UI.Gtk (Markup, escapeMarkup)
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
import Prelude hiding (catch)
import Text.Printf (printf)

import System.Information.X11DesktopInfo

type Listener = Event -> IO ()
type Filter = Atom
type SubscriptionList = IORef [(Listener, Filter)]

-- | Structure contanining functions to customize the pretty printing of
-- different widget elements.
data PagerConfig = PagerConfig
  { activeWindow     :: String -> Markup -- ^ the name of the active window.
  , activeLayout     :: String -> Markup -- ^ the currently active layout.
  , activeWorkspace  :: String -> Markup -- ^ the currently active workspace.
  , hiddenWorkspace  :: String -> Markup -- ^ inactive workspace with windows.
  , emptyWorkspace   :: String -> Markup -- ^ inactive workspace with no windows.
  , visibleWorkspace :: String -> Markup -- ^ all other visible workspaces (Xinerama or XRandR).
  , urgentWorkspace  :: String -> Markup -- ^ workspaces containing windows with the urgency hint set.
  , widgetSep        :: Markup -- ^ separator to use between desktop widgets in 'TaffyPager'.
  }

-- | Structure containing the state of the Pager.
data Pager = Pager
  { config  :: PagerConfig -- ^ the configuration settings.
  , clients :: SubscriptionList -- ^ functions to apply on incoming events depending on their types.
  }

-- | Default pretty printing options.
defaultPagerConfig :: PagerConfig
defaultPagerConfig   = PagerConfig
  { activeWindow     = colorize "green" "" . escape . shorten 40
  , activeLayout     = escape
  , activeWorkspace  = colorize "yellow" "" . wrap "[" "]" . escape
  , hiddenWorkspace  = escape
  , emptyWorkspace   = escape
  , visibleWorkspace = wrap "(" ")" . escape
  , urgentWorkspace  = colorize "red" "yellow" . escape
  , widgetSep        = " : "
  }

-- | Creates a new Pager component (wrapped in the IO Monad) that can be
-- used by widgets for subscribing X11 events.
pagerNew :: PagerConfig -> IO Pager
pagerNew cfg = do
  ref <- newIORef []
  let pager = Pager cfg ref
  forkIO $ withDefaultCtx $ eventLoop (handleEvent ref)
  return pager
    where handleEvent :: SubscriptionList -> Event -> IO ()
          handleEvent ref event = do
            listeners <- readIORef ref
            mapM_ (notify event) listeners

-- | Passes the given Event to the given Listener, but only if it was
-- registered for that type of events via 'subscribe'.
notify :: Event -> (Listener, Filter) -> IO ()
notify event (listener, filter) =
  case event of
    PropertyEvent _ _ _ _ _ atom _ _ ->
      when (atom == filter) $ catch (listener event) ignoreIOException
    _ -> return ()

-- | Registers the given Listener as a subscriber of events of the given
-- type: whenever a new event of the type with the given name arrives to
-- the Pager, it will execute Listener on it.
subscribe :: Pager -> Listener -> String -> IO ()
subscribe pager listener filterName = do
  filter <- withDefaultCtx $ getAtom filterName
  registered <- readIORef (clients pager)
  let next = (listener, filter)
  writeIORef (clients pager) (next : registered)

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()

-- | Creates markup with the given foreground and background colors and the
-- given contents.
colorize :: String -- ^ Foreground color.
         -> String -- ^ Background color.
         -> String -- ^ Contents.
         -> Markup
colorize fg bg = printf "<span%s%s>%s</span>" (attr "fg" fg) (attr "bg" bg)
  where attr name value
          | null value = ""
          | otherwise  = printf " %scolor=\"%s\"" name value

-- | Limit a string to a certain length, adding "..." if truncated.
shorten :: Int -> String -> String
shorten l s
  | length s <= l = s
  | l >= 3        = take (l - 3) s ++ "..."
  | otherwise     = "..."

-- | Wrap the given string in the given delimiters.
wrap :: String -- ^ Left delimiter.
     -> String -- ^ Right delimiter.
     -> String -- ^ Output string.
     -> String
wrap open close s = open ++ s ++ close

-- | Escape strings so that they can be safely displayed by Pango in the
-- bar widget
escape :: String -> String
escape = escapeMarkup