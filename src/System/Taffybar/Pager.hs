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
  ( Pager (..)
  , PagerConfig (..)
  , PagerIO
  , defaultPagerConfig
  , pagerNew
  , subscribe
  , colorize
  , liftPagerX11
  , liftPagerX11Def
  , runWithPager
  , shorten
  , wrap
  , escape
  ) where

import qualified Control.Concurrent.MVar as MV
import Control.Exception
import Control.Monad.Reader
import Data.Unique
import System.Information.SafeX11
import qualified Data.Map as M
import Graphics.UI.Gtk (escapeMarkup)
import System.Information.EWMHDesktopInfo
import System.Information.X11DesktopInfo
import qualified System.Taffybar.Context as TContext
import Text.Printf (printf)

-- | Structure contanining functions to customize the pretty printing of
-- different widget elements.
data PagerConfig = PagerConfig
  { activeWindow            :: String -> String
  -- ^ the name of the active window.
  , activeLayout            :: String -> String
  -- ^ the currently active layout.
  , activeLayoutIO          :: String -> IO String
  -- ^ IO action to modify active layout.
  , activeWorkspace         :: String -> String
  -- ^ the currently active workspace.
  , hiddenWorkspace         :: String -> String
  -- ^ inactive workspace with windows.
  , emptyWorkspace          :: String -> String
  -- ^ inactive workspace with no windows.
  , visibleWorkspace        :: String -> String
  -- ^ all other visible workspaces (Xinerama or XRandR).
  , urgentWorkspace         :: String -> String
  -- ^ workspaces containing windows with the urgency hint set.
  , widgetSep               :: String
  -- ^ separator to use between desktop widgets in 'TaffyPager'.
  , workspaceBorder         :: Bool
  -- ^ wrap workspace buttons in a frame
  , workspaceGap            :: Int
  -- ^ space in pixels between workspace buttons
  , workspacePad            :: Bool
  -- ^ pad workspace name in button
  , useImages               :: Bool
  -- ^ use images in the workspace switcher
  , imageSize               :: Int
  -- ^ image height and width in pixels
  , fillEmptyImages         :: Bool
  -- ^ fill empty images instead of clearing them
  , customIcon              :: Bool -> String -> String -> Maybe FilePath
  -- ^ get custom icon based on: has-EWMH-icon, window-title, window-class
  , windowSwitcherFormatter :: M.Map WorkspaceIdx String -> X11WindowHandle -> String
  -- ^ title windows for WindowSwitcher
  }

-- | Structure containing the state of the Pager.
data Pager = Pager
  { config  :: PagerConfig -- ^ the configuration settings.
  , tContext :: TContext.Context
  }

type PagerIO a = ReaderT Pager IO a

liftPagerX11 :: X11Property a -> PagerIO a
liftPagerX11 prop = ask >>= lift . flip runWithPager prop

liftPagerX11Def :: a -> X11Property a -> PagerIO a
liftPagerX11Def def prop = liftPagerX11 $ postX11RequestSyncProp prop def

runWithPager :: Pager -> X11Property a -> IO a
runWithPager pager prop = do
  x11Ctx <- MV.readMVar $ TContext.x11ContextVar $ tContext pager
  runReaderT prop x11Ctx

-- | Default pretty printing options.
defaultPagerConfig :: PagerConfig
defaultPagerConfig = PagerConfig
  { activeWindow            = escape . shorten 40
  , activeLayout            = escape
  , activeLayoutIO          = return
  , activeWorkspace         = colorize "yellow" "" . wrap "[" "]" . escape
  , hiddenWorkspace         = escape
  , emptyWorkspace          = const ""
  , visibleWorkspace        = wrap "(" ")" . escape
  , urgentWorkspace         = colorize "red" "yellow" . escape
  , widgetSep               = " : "
  , workspaceBorder         = False
  , workspaceGap            = 0
  , workspacePad            = True
  , useImages               = False
  , imageSize               = 16
  , fillEmptyImages         = False
  , customIcon              = \_ _ _ -> Nothing
  , windowSwitcherFormatter = defaultFormatEntry
  }

-- | Build the name to display in the list of windows by prepending the name
-- of the workspace it is currently in to the name of the window itself
defaultFormatEntry
  :: M.Map WorkspaceIdx String -- ^ List $ names of all available workspaces
  -> X11WindowHandle -- ^ Handle of the window to name
  -> String
defaultFormatEntry wsNames ((ws, wtitle, _), _) =
  printf "%s: %s " wsName $ nonEmpty wtitle
  where
    wsName = M.findWithDefault ("WS#" ++ show wsN) ws wsNames
    WSIdx wsN = ws
    nonEmpty x =
      case x of
        [] -> "(nameless window)"
        _ -> x

-- | Creates a new Pager component (wrapped in the IO Monad) that can be
-- used by widgets for subscribing X11 events.
pagerNew :: PagerConfig -> IO Pager
pagerNew cfg = Pager cfg <$> TContext.buildEmptyContext

-- | Registers the given Listener as a subscriber of events of the given
-- type: whenever a new event of the type with the given name arrives to
-- the Pager, it will execute Listener on it.
subscribe :: Pager -> (Event -> IO ()) -> String -> IO Unique
subscribe pager listener filterName =
  runReaderT (TContext.subscribeToEvents [filterName] (lift . listener)) $ tContext pager

-- | Creates markup with the given foreground and background colors and the
-- given contents.
colorize :: String -- ^ Foreground color.
         -> String -- ^ Background color.
         -> String -- ^ Contents.
         -> String
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
