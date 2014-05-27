{-# LANGUAGE RankNTypes #-}

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
  ( WorkspaceInfo (..)
  , WSVisibility (..)
  , wsiEmpty
  , Pager (config)
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
import Control.Exception as E
import Control.Monad.Reader
import Data.IORef
import Data.Traversable
import Graphics.UI.Gtk (Markup, escapeMarkup)
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
import Text.Printf (printf)

import System.Information.X11DesktopInfo

type Listener = Event -> IO ()
type Filter = Atom
type SubscriptionList = IORef [(Listener, Filter)]

data WorkspaceInfo = WSInfo { wsiName       :: String -- ^ the name of the workspace
                            , wsiWindows    :: Int    -- ^ how many windows are on the workspace?
                            , wsiVisibility :: WSVisibility -- ^ is the workspace visible?
                            , wsiUrgent     :: Bool   -- ^ do any of the windows have the urgent hint set?
                            }

data WSVisibility = Active   -- ^ workspace is active
                  | Visible  -- ^ workspace is visible
                  | Hidden   -- ^ workspace is not visible
                  deriving (Show, Ord, Eq, Bounded, Enum)

wsiEmpty :: WorkspaceInfo -> Bool
wsiEmpty = (==0) . wsiWindows

-- | Structure contanining functions to customize the pretty printing of
-- different widget elements.
data PagerConfig = PagerConfig
  { activeWindow     :: String -> Markup -- ^ the name of the active window.
  , activeLayout     :: String -> Markup -- ^ the currently active layout.
  , markupWorkspaces :: forall f. Traversable f => f WorkspaceInfo -> f Markup -- ^ markup for a list of workspaces.
  , widgetSep        :: Markup           -- ^ separator to use between desktop widgets in 'TaffyPager'.
  }

-- | Structure containing the state of the Pager.
data Pager = Pager
  { config  :: PagerConfig -- ^ the configuration settings.
  , clients :: SubscriptionList -- ^ functions to apply on incoming events depending on their types.
  }

-- | Default pretty printing options.
defaultPagerConfig :: PagerConfig
defaultPagerConfig   = PagerConfig
  { activeWindow     = escape . shorten 40
  , activeLayout     = escape
  , markupWorkspaces = defaultMarkupWorkspaces
  , widgetSep        = " : "
  }

-- | Default workspace markup
defaultMarkupWorkspaces :: Traversable f => f WorkspaceInfo -> f Markup
defaultMarkupWorkspaces = fmap f
  where
    f ws@(WSInfo {wsiName=name, wsiVisibility=vis})
      | wsiUrgent ws   = colorize "red" "yellow" $ escape name
      | Active <- vis  = colorize "yellow" "" $ wrap "[" "]" $ escape name
      | Visible <- vis = wrap "(" ")" $ escape name
      | wsiEmpty ws    = escape name
      | otherwise      = escape name

-- | Creates a new Pager component (wrapped in the IO Monad) that can be
-- used by widgets for subscribing X11 events.
pagerNew :: PagerConfig -> IO Pager
pagerNew cfg = do
  ref <- newIORef []
  let pager = Pager cfg ref
  _ <- forkIO $ withDefaultCtx $ eventLoop (handleEvent ref)
  return pager
    where handleEvent :: SubscriptionList -> Event -> IO ()
          handleEvent ref event = do
            listeners <- readIORef ref
            mapM_ (notify event) listeners

-- | Passes the given Event to the given Listener, but only if it was
-- registered for that type of events via 'subscribe'.
notify :: Event -> (Listener, Filter) -> IO ()
notify event (listener, eventFilter) =
  case event of
    PropertyEvent _ _ _ _ _ atom _ _ ->
      when (atom == eventFilter) $ E.catch (listener event) ignoreIOException
    _ -> return ()

-- | Registers the given Listener as a subscriber of events of the given
-- type: whenever a new event of the type with the given name arrives to
-- the Pager, it will execute Listener on it.
subscribe :: Pager -> Listener -> String -> IO ()
subscribe pager listener filterName = do
  eventFilter <- withDefaultCtx $ getAtom filterName
  registered <- readIORef (clients pager)
  let next = (listener, eventFilter)
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
