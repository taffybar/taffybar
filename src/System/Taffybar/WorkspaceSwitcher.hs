{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.WorkspaceSwitcher
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Composite widget that displays all currently configured workspaces and
-- allows to switch to any of them by clicking on its label. Supports also
-- urgency hints and (with an additional hook) display of other visible
-- workspaces besides the active one (in Xinerama or XRandR installations).
--
-- N.B. If you're just looking for a drop-in replacement for the
-- "System.Taffybar.XMonadLog" widget that is clickable and doesn't require
-- DBus, you may want to see first "System.Taffybar.TaffyPager".
--
-----------------------------------------------------------------------------

module System.Taffybar.WorkspaceSwitcher (
  -- * Usage
  -- $usage
  wspaceSwitcherNew
) where

import Control.Applicative
import qualified Control.Concurrent.MVar as MV
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List ((\\), findIndices, sortBy)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Foreign.C.Types (CUChar(..))
import Foreign.Marshal.Array (newArray)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.X11.Xlib.Extras

import Prelude

import System.Taffybar.IconImages hiding (selectEWMHIcon)
import System.Taffybar.Pager
import System.Information.EWMHDesktopInfo

type Desktop = [Workspace]
data Workspace = Workspace { label     :: Gtk.Label
                           , image     :: Gtk.Image
                           , border    :: Gtk.EventBox
                           , container :: Gtk.EventBox
                           , name      :: String
                           , urgent    :: Bool
                           }
type WindowSet = [(WorkspaceIdx, [X11Window])]
type WindowInfo = Maybe (String, String, [EWMHIcon])
type CustomIconF = String -> String -> Maybe FilePath
type ImageChoice = (Maybe EWMHIcon, Maybe FilePath, Maybe ColorRGBA)

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
-- Urgency hooks are not required for the urgency hints displaying to work
-- (since it is also based on desktop events), but if you use @focusUrgent@
-- you may want to keep the \"@withUrgencyHook NoUrgencyHook@\" anyway.
--
-- Unfortunately, in multiple monitor installations EWMH does not provide a
-- way to determine what desktops are shown in secondary displays. Thus, if
-- you have more than one monitor you may want to additionally install the
-- "System.Taffybar.Hooks.PagerHints" hook in your @xmonad.hs@:
--
-- > import System.Taffybar.Hooks.PagerHints (pagerHints)
-- > main = do
-- >   xmonad $ ewmh $ pagerHints $ defaultConfig
-- > ...
--
-- Once you've properly configured @xmonad.hs@, you can use the widget in
-- your @taffybar.hs@ file:
--
-- > import System.Taffybar.WorkspaceSwitcher
-- > main = do
-- >   pager <- pagerNew defaultPagerConfig
-- >   let wss = wspaceSwitcherNew pager
--
-- now you can use @wss@ as any other Taffybar widget.

-- | Create a new WorkspaceSwitcher widget that will use the given Pager as
-- its source of events.
wspaceSwitcherNew :: Pager -> IO Gtk.Widget
wspaceSwitcherNew pager = do
  switcher <- Gtk.hBoxNew False (workspaceGap (config pager))
  desktop  <- getDesktop pager
  deskRef  <- MV.newMVar desktop
  populateSwitcher switcher deskRef

  -- These callbacks need to use postGUIAsync since they run in
  -- another thread
  let cfg = config pager
      activecb = activeCallback cfg deskRef
      redrawcb = redrawCallback pager deskRef switcher
      urgentcb = urgentCallback cfg deskRef
  subscribe pager activecb "_NET_CURRENT_DESKTOP"
  subscribe pager activecb "_NET_WM_DESKTOP"
  subscribe pager redrawcb "_NET_DESKTOP_NAMES"
  subscribe pager redrawcb "_NET_NUMBER_OF_DESKTOPS"
  subscribe pager urgentcb "WM_HINTS"

  return $ Gtk.toWidget switcher

-- | List of indices of all available workspaces.
allWorkspaces :: Desktop -> [WorkspaceIdx]
allWorkspaces desktop = map WSIdx [0 .. length desktop - 1]

-- | List of indices of all the workspaces that contain at least one window.
nonEmptyWorkspaces :: IO [WorkspaceIdx]
nonEmptyWorkspaces = withDefaultCtx $ mapM getWorkspace =<< getWindows

-- | Return a list of Workspace data instances.
getDesktop :: Pager -> IO Desktop
getDesktop pager = do
  names  <- map snd <$> withDefaultCtx getWorkspaceNames
  mapM (createWorkspace pager) names

-- | Return a Workspace data instance, with the unmarked name,
-- label widget, and image widget.
createWorkspace :: Pager -> String -> IO Workspace
createWorkspace _pager wname = do
  lbl <- createLabel wname
  img <- Gtk.imageNew
  brd <- Gtk.eventBoxNew
  con <- Gtk.eventBoxNew

  let useBorder = workspaceBorder (config _pager)
  Gtk.eventBoxSetVisibleWindow brd useBorder
  Gtk.containerSetBorderWidth con (if useBorder then 2 else 0)

  return $ Workspace lbl img brd con wname False

-- | Take an existing Desktop IORef and update it if necessary, store the result
-- in the IORef, then return True if the reference was actually updated, False
-- otherwise.
updateDesktop :: Pager -> MV.MVar Desktop -> IO Bool
updateDesktop pager deskRef = do
  wsnames <- withDefaultCtx getWorkspaceNames
  MV.modifyMVar deskRef $ \desktop ->
    case map snd wsnames /= map name desktop of
      True -> do
        desk' <- getDesktop pager
        return (desk', True)
      False -> return (desktop, False)

-- | Clean up the given box, then fill it up with the buttons for the current
-- state of the desktop.
populateSwitcher :: Gtk.BoxClass box => box -> MV.MVar Desktop -> IO ()
populateSwitcher switcher deskRef = do
  containerClear switcher
  desktop <- MV.readMVar deskRef
  mapM_ (addButton switcher desktop) (allWorkspaces desktop)
  Gtk.widgetShowAll switcher

-- | Build a suitable callback function that can be registered as Listener
-- of "_NET_CURRENT_DESKTOP" standard events. It will track the position of
-- the active workspace in the desktop.
activeCallback :: PagerConfig -> MV.MVar Desktop -> Event -> IO ()
activeCallback cfg deskRef _ = Gtk.postGUIAsync $ do
  curr <- withDefaultCtx getVisibleWorkspaces
  desktop <- MV.readMVar deskRef
  case curr of
    visible : _ | Just ws <- getWS desktop visible -> do
      when (urgent ws) $ toggleUrgent deskRef visible False
      transition cfg desktop curr
    _ -> return ()

-- | Build a suitable callback function that can be registered as Listener
-- of "WM_HINTS" standard events. It will display in a different color any
-- workspace (other than the active one) containing one or more windows
-- with its urgency hint set.
urgentCallback :: PagerConfig -> MV.MVar Desktop -> Event -> IO ()
urgentCallback cfg deskRef event = Gtk.postGUIAsync $ do
  desktop <- MV.readMVar deskRef
  withDefaultCtx $ do
    let window = ev_window event
        pad = if workspacePad cfg then prefixSpace else id
    isUrgent <- isWindowUrgent window
    when isUrgent $ do
      this <- getCurrentWorkspace
      that <- getWorkspace window
      when (this /= that) $ liftIO $ do
        toggleUrgent deskRef that True
        mark desktop pad (urgentWorkspace cfg) that

-- | Build a suitable callback function that can be registered as Listener
-- of "_NET_NUMBER_OF_DESKTOPS" standard events. It will handle dynamically
-- adding and removing workspaces.
redrawCallback :: Gtk.BoxClass box => Pager -> MV.MVar Desktop -> box -> Event -> IO ()
redrawCallback pager deskRef box _ = Gtk.postGUIAsync $ do
  -- updateDesktop indirectly invokes some gtk functions, so it also
  -- needs to be guarded by postGUIAsync
  deskChanged <- updateDesktop pager deskRef
  when deskChanged $ populateSwitcher box deskRef

-- | Remove all children of a container.
containerClear :: Gtk.ContainerClass self => self -> IO ()
containerClear c = Gtk.containerForeach c (Gtk.containerRemove c)

-- | Create a label widget from the given String.
createLabel :: String -> IO Gtk.Label
createLabel markup = do
  lbl <- Gtk.labelNew (Nothing :: Maybe String)
  Gtk.labelSetMarkup lbl markup
  return lbl

-- | Get the workspace corresponding to the given 'WorkspaceIdx' on the given desktop
getWS :: Desktop -> WorkspaceIdx -> Maybe Workspace
getWS desktop (WSIdx idx)
  | length desktop > idx = Just (desktop !! idx)
  | otherwise            = Nothing

-- | Build a new clickable event box containing the Label widget that
-- corresponds to the given index, and add it to the given container.
addButton :: Gtk.BoxClass self
          => self         -- ^ Graphical container.
          -> Desktop      -- ^ List of all workspace Labels available.
          -> WorkspaceIdx -- ^ Index of the workspace to use.
          -> IO ()
addButton switcherHbox desktop idx
  | Just ws <- getWS desktop idx = do
    let lbl = label ws
    let img = image ws
    let brd = border ws
    let con = container ws
    btnParentEbox <- Gtk.eventBoxNew
    iconLabelBox <- Gtk.hBoxNew False 0

    Gtk.boxPackStart switcherHbox btnParentEbox Gtk.PackNatural 0
    Gtk.containerAdd btnParentEbox brd
    Gtk.containerAdd brd con
    Gtk.containerAdd con iconLabelBox
    Gtk.containerAdd iconLabelBox lbl
    Gtk.containerAdd iconLabelBox img

    Gtk.widgetSetName btnParentEbox $ name ws
    Gtk.eventBoxSetVisibleWindow btnParentEbox False
    _ <- Gtk.on btnParentEbox Gtk.buttonPressEvent $ switch idx
    _ <- Gtk.on btnParentEbox Gtk.scrollEvent $ do
      dir <- Gtk.eventScrollDirection
      case dir of
        Gtk.ScrollUp    -> switchOne True (length desktop - 1)
        Gtk.ScrollLeft  -> switchOne True (length desktop - 1)
        Gtk.ScrollDown  -> switchOne False (length desktop - 1)
        Gtk.ScrollRight -> switchOne False (length desktop - 1)
    return ()

  | otherwise = return ()

-- | Re-mark all workspace labels.
transition :: PagerConfig    -- ^ Configuration settings.
           -> Desktop        -- ^ All available Labels with their default values.
           -> [WorkspaceIdx] -- ^ Currently visible workspaces (first is active).
           -> IO ()
transition cfg desktop wss = do
  nonEmpty <- fmap (filter (>=WSIdx 0)) nonEmptyWorkspaces
  let urgentWs = map WSIdx $ findIndices urgent desktop
      allWs    = (allWorkspaces desktop) \\ urgentWs
      nonEmptyWs = nonEmpty \\ urgentWs
      pad = if workspacePad cfg then prefixSpace else id
  mapM_ (mark desktop pad $ hiddenWorkspace cfg) nonEmptyWs
  mapM_ (setWidgetNames desktop "hidden") nonEmptyWs
  mapM_ (mark desktop pad $ emptyWorkspace cfg) (allWs \\ nonEmpty)
  mapM_ (setWidgetNames desktop "empty") (allWs \\ nonEmpty)
  case wss of
    active:rest -> do
      mark desktop pad (activeWorkspace cfg) active
      setWidgetNames desktop "active" active
      mapM_ (mark desktop pad $ visibleWorkspace cfg) rest
      mapM_ (setWidgetNames desktop "visible") rest
    _ -> return ()
  mapM_ (mark desktop pad $ urgentWorkspace cfg) urgentWs
  mapM_ (setWidgetNames desktop "urgent") urgentWs

  let useImg = useImages cfg
      fillEmpty = fillEmptyImages cfg
      imgSize = imageSize cfg
      customIconF = customIcon cfg
      preferCustom = preferCustomIcon cfg
  when useImg $ updateImages desktop imgSize fillEmpty preferCustom customIconF

-- | Update the GTK images using X properties.
updateImages :: Desktop -> Int -> Bool -> Bool -> CustomIconF -> IO ()
updateImages desktop imgSize fillEmpty preferCustom customIconF = do
  windowSet <- getWindowSet (allWorkspaces desktop)
  lastWinInfo <- getLastWindowInfo windowSet
  let images = map image desktop
      fillColor = if fillEmpty then Just (0, 0, 0, 0) else Nothing
      imageChoices = getImageChoices lastWinInfo customIconF fillColor imgSize
  zipWithM_ (setImage imgSize preferCustom) images imageChoices

-- | Get EWMHIcons, custom icon files, and fill colors based on the window info.
getImageChoices :: [WindowInfo] -> CustomIconF -> Maybe ColorRGBA -> Int -> [ImageChoice]
getImageChoices lastWinInfo customIconF fillColor imgSize = zip3 icons files colors
  where icons = map (selectEWMHIcon imgSize) lastWinInfo
        files = map (selectCustomIconFile customIconF) lastWinInfo
        colors = map (\_ -> fillColor) lastWinInfo

-- | Select the icon with the smallest height that is larger than imgSize,
-- or if none such icons exist, select the icon with the largest height.
selectEWMHIcon :: Int -> WindowInfo -> Maybe EWMHIcon
selectEWMHIcon imgSize (Just (_, _, icons)) = listToMaybe prefIcon
  where sortedIcons = sortOn height icons
        smallestLargerIcon = take 1 $ dropWhile ((<=imgSize).height) sortedIcons
        largestIcon = take 1 $ reverse sortedIcons
        prefIcon = smallestLargerIcon ++ largestIcon
        sortOn f = sortBy (comparing f)
selectEWMHIcon _ _ = Nothing

-- | Select a file using customIcon config.
selectCustomIconFile :: CustomIconF -> WindowInfo -> Maybe FilePath
selectCustomIconFile customIconF (Just (wTitle, wClass, _)) = customIconF wTitle wClass
selectCustomIconFile _ _ = Nothing

-- | Sets an image based on the image choice (EWMHIcon, custom file, and fill color).
setImage :: Int -> Bool -> Gtk.Image -> ImageChoice -> IO ()
setImage imgSize preferCustom img imgChoice = setImgAct imgChoice preferCustom
  where setImgAct (_, Just file, _) True = setImageFromFile img imgSize file
        setImgAct (Just icon, _, _) _    = setImageFromEWMHIcon img imgSize icon
        setImgAct (_, Just file, _) _    = setImageFromFile img imgSize file
        setImgAct (_, _, Just color) _   = setImageFromColor img imgSize color
        setImgAct _ _                    = Gtk.imageClear img

-- | Create a pixbuf from the pixel data in an EWMHIcon,
-- scale it square, and set it in a GTK Image.
setImageFromEWMHIcon :: Gtk.Image -> Int -> EWMHIcon -> IO ()
setImageFromEWMHIcon img imgSize EWMHIcon {width=w, height=h, pixelsARGB=px} = do
  let pixelsPerRow = w
      bytesPerPixel = 4
      rowStride = pixelsPerRow * bytesPerPixel
      sampleBits = 8
      hasAlpha = True
      colorspace = Gtk.ColorspaceRgb
      bytesRGBA = pixelsARGBToBytesRGBA px
  cPtr <- newArray $ map CUChar bytesRGBA
  pixbuf <- Gtk.pixbufNewFromData cPtr colorspace hasAlpha sampleBits w h rowStride
  scaledPixbuf <- scalePixbuf imgSize pixbuf
  Gtk.imageSetFromPixbuf img scaledPixbuf

-- | Create a pixbuf from a file,
-- scale it square, and set it in a GTK Image.
setImageFromFile :: Gtk.Image -> Int -> FilePath -> IO ()
setImageFromFile img imgSize file = do
  pixbuf <- Gtk.pixbufNewFromFileAtScale file imgSize imgSize False
  scaledPixbuf <- scalePixbuf imgSize pixbuf
  Gtk.imageSetFromPixbuf img scaledPixbuf

-- | Create a pixbuf with the indicated RGBA color,
-- scale it square, and set it in a GTK Image.
setImageFromColor :: Gtk.Image -> Int -> ColorRGBA -> IO ()
setImageFromColor img imgSize (r,g,b,a) = do
  let sampleBits = 8
      hasAlpha = True
      colorspace = Gtk.ColorspaceRgb
  pixbuf <- Gtk.pixbufNew colorspace hasAlpha sampleBits imgSize imgSize
  Gtk.pixbufFill pixbuf r g b a
  scaledPixbuf <- scalePixbuf imgSize pixbuf
  Gtk.imageSetFromPixbuf img scaledPixbuf

-- | Get window title, class, and icons for the last window in each workspace.
getLastWindowInfo :: WindowSet -> IO [WindowInfo]
getLastWindowInfo windowSet = mapM getWindowInfo lastWins
  where wsIdxs = map fst windowSet
        lastWins = map lastWin wsIdxs
        wins wsIdx = snd $ head $ filter ((==wsIdx).fst) windowSet
        lastWin wsIdx = listToMaybe $ reverse $ wins wsIdx

-- | Get window title, class, and EWMHIcons for the given window.
getWindowInfo :: Maybe X11Window -> IO WindowInfo
getWindowInfo Nothing = return Nothing
getWindowInfo (Just w) = withDefaultCtx $ do
  wTitle <- getWindowTitle w
  wClass <- getWindowClass w
  wIcon <- getWindowIcons w
  return $ Just (wTitle, wClass, wIcon)

-- | Get a list of windows for each workspace.
getWindowSet :: [WorkspaceIdx] -> IO WindowSet
getWindowSet wsIdxs = do
  windows <- withDefaultCtx getWindows
  workspaces <- mapM (withDefaultCtx.getWorkspace) windows
  let wsWins = zip workspaces windows
  return $ map (\wsIdx -> (wsIdx, lookupAll wsIdx wsWins)) wsIdxs
  where lookupAll x xs = map snd $ filter (((==)x).fst) xs

-- | Apply the given marking function to the Label of the workspace with
-- the given index.
mark :: Desktop            -- ^ List of all available labels.
     -> (String -> String) -- ^ Padding function.
     -> (String -> String) -- ^ Marking function.
     -> WorkspaceIdx       -- ^ Index of the Label to modify.
     -> IO ()
mark desktop pad decorate wsIdx
  | Just ws <- getWS desktop wsIdx =
    Gtk.postGUIAsync $ Gtk.labelSetMarkup (label ws) $ pad $ decorate (name ws)
  | otherwise = return ()

-- | Prefix the string with a space unless the string is empty.
prefixSpace :: String -> String
prefixSpace "" = ""
prefixSpace s = " " ++ s

-- | Set the widget names of the workspace button components:
-- border    => Workspace-Border-<WORKSPACE_NAME>-<WORKSPACE_STATE>
-- image     => Workspace-Image-<WORKSPACE_NAME>-<WORKSPACE_STATE>
-- container => Workspace-Container-<WORKSPACE_NAME>-<WORKSPACE_STATE>
-- label     => Workspace-Label-<WORKSPACE_NAME>-<WORKSPACE_STATE>
setWidgetNames :: Desktop -> String -> WorkspaceIdx -> IO ()
setWidgetNames desktop workspaceState wsIdx
  | Just ws <- getWS desktop wsIdx = do
      Gtk.widgetSetName (label ws)     (widgetName "Label"     (name ws))
      Gtk.widgetSetName (image ws)     (widgetName "Image"     (name ws))
      Gtk.widgetSetName (border ws)    (widgetName "Border"    (name ws))
      Gtk.widgetSetName (container ws) (widgetName "Container" (name ws))
  | otherwise = return ()
  where widgetName widget wsName = "Workspace"
                                   ++ "-" ++ widget
                                   ++ "-" ++ wsName
                                   ++ "-" ++ workspaceState

-- | Switch to the workspace with the given index.
switch :: (MonadIO m) => WorkspaceIdx -> m Bool
switch idx = do
  liftIO $ withDefaultCtx (switchToWorkspace idx)
  return True

-- | Switch to one workspace up or down given a boolean direction and the last workspace
switchOne :: (MonadIO m) => Bool -> Int -> m Bool
switchOne dir end = do
  liftIO $ withDefaultCtx (if dir then switchOneWorkspace dir end else switchOneWorkspace dir end)
  return True

-- | Modify the Desktop inside the given IORef, so that the Workspace at the
-- given index has its "urgent" flag set to the given value.
toggleUrgent :: MV.MVar Desktop -- ^ MVar to modify.
             -> WorkspaceIdx  -- ^ Index of the Workspace to replace.
             -> Bool          -- ^ New value of the "urgent" flag.
             -> IO ()
toggleUrgent deskRef (WSIdx idx) isUrgent =
  MV.modifyMVar_ deskRef $ \desktop -> do
    let ws = desktop !! idx
    case length desktop > idx of
      True | isUrgent /= urgent ws -> do
               let ws' = ws { urgent = isUrgent }
                   (ys, zs) = splitAt idx desktop
               case zs of
                 _ : rest -> return $ ys ++ (ws' : rest)
                 _ -> return (ys ++ [ws'])
      _ -> return desktop
