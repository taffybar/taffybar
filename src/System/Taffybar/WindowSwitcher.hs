-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.WindowSwitcher
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Text widget that shows the title of the currently focused window and
-- that, when clicked with the mouse, displays a pop-up with a list of all
-- currently open windows that allows to switch to any of them.
--
-- N.B. If you're just looking for a drop-in replacement for the
-- "System.Taffybar.XMonadLog" widget that is clickable and doesn't require
-- DBus, you may want to see first "System.Taffybar.TaffyPager".
--
-----------------------------------------------------------------------------

module System.Taffybar.WindowSwitcher (
  -- * Usage
  -- $usage
  windowSwitcherNew
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as M
import Graphics.X11.Xlib.Extras (Event)
import System.Information.EWMHDesktopInfo
import System.Taffybar.Pager
import System.Taffybar.Widgets.Util

-- $usage
--
-- This widget requires that the EwmhDesktops hook from the XMonadContrib
-- project be installed in your @xmonad.hs@ file:
--
-- > import XMonad.Hooks.EwmhDesktops (ewmh)
-- > main = do
-- >   xmonad $ ewmh $ defaultConfig
-- > ...
--
-- Once you've properly configured @xmonad.hs@, you can use the widget in
-- your @taffybar.hs@ file:
--
-- > import System.Taffybar.WindowSwitcher
-- > main = do
-- >   pager <- pagerNew defaultPagerConfig
-- >   let wnd = windowSwitcherNew pager
--
-- now you can use @wnd@ as any other Taffybar widget.

-- | Create a new WindowSwitcher widget that will use the given Pager as
-- its source of events.
windowSwitcherNew :: Pager -> IO Widget
windowSwitcherNew pager = do
  label <- labelNew Nothing
  let cfg = config pager
      callback = pagerCallback cfg label
  subscribe pager callback "_NET_ACTIVE_WINDOW"
  widget <- assembleWidget label
  return widget

-- | Build a suitable callback function that can be registered as Listener
-- of "_NET_ACTIVE_WINDOW" standard events. It will keep track of the
-- currently focused window.
pagerCallback :: PagerConfig -> Label -> Event -> IO ()
pagerCallback cfg label _ = do
  title <- withDefaultCtx getActiveWindowTitle
  let decorate = activeWindow cfg
  postGUIAsync $ labelSetMarkup label (decorate title)

-- | Build the graphical representation of the widget.
assembleWidget :: Label -> IO Widget
assembleWidget l = do
  box <- eventBoxNew
  containerAdd box l
  eventBoxSetVisibleWindow box False
  ref <- newIORef []
  _ <- on box buttonPressEvent $ onClick [SingleClick] (toggleSelector l ref)
  widgetShowAll box
  return (toWidget box)

-- | Either create a new pop-up window (aka "selector") if none is
-- currently present, or destroy the one being currently displayed.
toggleSelector :: Label -- ^ Parent of the pop-up window to create.
               -> IORef [Window] -- ^ Last created pop-up window (if any)
               -> IO Bool
toggleSelector label ref = do
  win <- readIORef ref
  case win of
    x:xs -> killSelector x ref
    _    -> do
      selector <- createSelector ref
      case selector of
        Just sel -> do
          title <- labelGetText label
          attachPopup label title sel
          displayPopup label sel
        Nothing -> return ()
  return True

-- | Build a new pop-up containing the titles of all currently open
-- windows, and assign it as a singleton list to the given IORef.
createSelector :: IORef [Window] -> IO (Maybe Window)
createSelector ref = do
  handles  <- withDefaultCtx getWindowHandles
  if length handles <= 0
    then return Nothing
    else do
      selector <- windowNew
      list     <- listStoreNew (map fst handles)
      view     <- makeTreeView list
      column   <- makeColumn list

      M.treeViewAppendColumn view column
      sel <- M.treeViewGetSelection view
      M.onSelectionChanged sel $ do
        handlePick sel list handles
        killSelector selector ref
      set selector [ containerChild := view ]
      _ <- on selector deleteEvent $ killSelector selector ref >> return False
      _ <- on selector focusOutEvent $ killSelector selector ref >> return False

      writeIORef ref [selector]
      return (Just selector)

-- | Destroy given pop-up and clean-up the given IORef.
killSelector :: (MonadIO m) => Window -> IORef[Window] -> m ()
killSelector window ref = liftIO $ do
  writeIORef ref []
  postGUIAsync (widgetDestroy window)

-- | Build a new TreeView from the given ListStore containing window
-- titles.
makeTreeView :: ListStore String -> IO TreeView
makeTreeView list = do
  treeview <- M.treeViewNewWithModel list
  M.treeViewSetHeadersVisible treeview False
  return treeview

-- | Build a new TreeViewColumn from the given ListStore containing window
-- titles.
makeColumn :: ListStore String -> IO TreeViewColumn
makeColumn list = do
  col <- M.treeViewColumnNew
  renderer <- M.cellRendererTextNew
  M.cellLayoutPackStart col renderer False
  M.cellLayoutSetAttributes col renderer list $ \ind -> [M.cellText := ind]
  return col

-- | Switch to the window selected by the user in the pop-up.
handlePick :: M.TreeSelection -- ^ Pop-up selection.
           -> ListStore String -- ^ List of all available windows.
           -> [(String, X11Window)] -- ^ Window titles and their IDs.
           -> IO ()
handlePick selection list handles = do
  row <- M.treeSelectionGetSelectedRows selection
  let idx = head (head row)
      wh = snd (handles !! idx)
  withDefaultCtx (focusWindow wh)
  return ()