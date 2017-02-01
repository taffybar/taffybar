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
import Data.Maybe
import Data.Ord (comparing)
import Data.Word (Word8)
import Foreign.C.Types (CUChar(..))
import Foreign.Marshal.Array (newArray)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.X11.Xlib.Extras

import Prelude

import System.Taffybar.Pager
import System.Information.EWMHDesktopInfo

type Desktop = [Workspace]
data Workspace = Workspace { button    :: Gtk.EventBox
                           , container :: Gtk.HBox
                           , label     :: Gtk.Label
                           , images    :: [Gtk.Image]
                           , border    :: Maybe Gtk.Frame
                           , name      :: String
                           , urgent    :: Bool
                           }
type WindowSet = [(WorkspaceIdx, [X11Window])]
type WindowInfo = (String, String, [EWMHIcon])
type ColorRGBA = (Word8, Word8, Word8, Word8)
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
      activefastcb = activeFastCallback cfg deskRef
      redrawcb = redrawCallback pager deskRef switcher
      urgentcb = urgentCallback cfg deskRef
  subscribe pager activecb "_NET_CURRENT_DESKTOP"
  subscribe pager activefastcb "_NET_WM_DESKTOP"
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
  contents <- Gtk.hBoxNew False 0
  ebox <- Gtk.eventBoxNew
  let pconfig = config _pager
      imagesToMake = (imageCount pconfig)
  imgs <- mapM (\_ -> Gtk.imageNew) [1..imagesToMake]
  Gtk.containerAdd contents lbl
  mapM_ (Gtk.containerAdd contents) imgs
  frm <- if workspaceBorder pconfig
           then fmap Just Gtk.frameNew
           else return Nothing
  return $ Workspace ebox contents lbl imgs frm wname False

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
      transition cfg True desktop curr
    _ -> return ()

-- | Build a suitable callback function that can be registered as Listener
-- of "_NET_WM_DESKTOP" standard events. It will track the position of
-- the active workspace in the desktop. It skips updating images.
activeFastCallback :: PagerConfig -> MV.MVar Desktop -> Event -> IO ()
activeFastCallback cfg deskRef _ = Gtk.postGUIAsync $ do
  curr <- withDefaultCtx getVisibleWorkspaces
  desktop <- MV.readMVar deskRef
  case curr of
    visible : _ | Just ws <- getWS desktop visible -> do
      when (urgent ws) $ toggleUrgent deskRef visible False
      transition cfg False desktop curr
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
containerClear cont = Gtk.containerForeach cont (Gtk.containerRemove cont)

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
addButton hbox desktop idx
  | Just ws <- getWS desktop idx = do
    let frm = border ws
        ebox = button ws
    _ <- Gtk.on ebox Gtk.buttonPressEvent $ switch idx
    _ <- Gtk.on ebox Gtk.scrollEvent $ do
      dir <- Gtk.eventScrollDirection
      case dir of
        Gtk.ScrollUp    -> switchOne True (length desktop - 1)
        Gtk.ScrollLeft  -> switchOne True (length desktop - 1)
        Gtk.ScrollDown  -> switchOne False (length desktop - 1)
        Gtk.ScrollRight -> switchOne False (length desktop - 1)
    case frm of
      Just f -> do
        Gtk.containerAdd f $ container ws
        Gtk.containerAdd ebox f
      Nothing -> Gtk.containerAdd ebox $ container ws
    Gtk.boxPackStart hbox ebox Gtk.PackNatural 0
  | otherwise = return ()

-- | Re-mark all workspace labels.
transition :: PagerConfig    -- ^ Configuration settings.
           -> Bool           -- ^ Update images as well
           -> Desktop        -- ^ All available Labels with their default values.
           -> [WorkspaceIdx] -- ^ Currently visible workspaces (first is active).
           -> IO ()
transition cfg updateImgs desktop wss = do
  nonEmpty <- fmap (filter (>=WSIdx 0)) nonEmptyWorkspaces

  let urgentWs = map WSIdx $ findIndices urgent desktop
      allWs    = (allWorkspaces desktop) \\ urgentWs
      nonEmptyWs = nonEmpty \\ urgentWs
      pad = if workspacePad cfg then prefixSpace else id

  mapM_ (mark desktop pad $ hiddenWorkspace cfg) nonEmptyWs
  mapM_ (setWidgetNamesByIndex desktop "hidden") nonEmptyWs
  mapM_ (mark desktop pad $ emptyWorkspace cfg) (allWs \\ nonEmpty)
  mapM_ (setWidgetNamesByIndex desktop "empty") (allWs \\ nonEmpty)

  case wss of
    active:rest -> do
      mark desktop pad (activeWorkspace cfg) active
      setWidgetNamesByIndex desktop "active" active
      mapM_ (mark desktop pad $ visibleWorkspace cfg) rest
      mapM_ (setWidgetNamesByIndex desktop "visible") rest
    _ -> return ()
  mapM_ (mark desktop pad $ urgentWorkspace cfg) urgentWs
  mapM_ (setWidgetNamesByIndex desktop "urgent") urgentWs

  let useImg = updateImgs && useImages cfg
      fillEmpty = fillEmptyImages cfg
      imgSize = imageSize cfg
      customIconF = customIcon cfg
      preferCustom = preferCustomIcon cfg
  when useImg $ updateImages desktop imgSize fillEmpty preferCustom customIconF

-- | Update the GTK images using X properties.
updateImages :: Desktop -> Int -> Bool -> Bool -> CustomIconF -> IO ()
updateImages desktop imgSize fillEmpty preferCustom customIconF = do
  windowSet <- getWindowSet (allWorkspaces desktop)
  windowInfo <- getWindowInfoByWS windowSet
  let imageSets = map images desktop
      fillColor = if fillEmpty then Just (0, 0, 0, 0) else Nothing
      imageChoices = getImageChoices windowInfo customIconF fillColor imgSize
  zipWithM_ (setImages imgSize preferCustom) imageSets $ map reverse imageChoices

-- | Get EWMHIcons, custom icon files, and fill colors based on the window info.
getImageChoices :: [[WindowInfo]] -> CustomIconF -> Maybe ColorRGBA -> Int -> [[ImageChoice]]
getImageChoices windowInfo customIconF fillColor imgSize = map (map getImageInfo) windowInfo
  where getImageInfo winfo = ( selectEWMHIcon imgSize winfo
                             , case winfo of
                                 (wTitle, wClass, _) -> customIconF wTitle wClass
                             , fillColor)

-- | Select the icon with the smallest height that is larger than imgSize,
-- or if none such icons exist, select the icon with the largest height.
selectEWMHIcon :: Int -> WindowInfo -> Maybe EWMHIcon
selectEWMHIcon imgSize (_, _, icons) = listToMaybe prefIcon
  where sortedIcons = sortOn height icons
        smallestLargerIcon = take 1 $ dropWhile ((<=imgSize).height) sortedIcons
        largestIcon = take 1 $ reverse sortedIcons
        prefIcon = smallestLargerIcon ++ largestIcon
        sortOn f = sortBy (comparing f)

setImages :: Int -> Bool -> [Gtk.Image] -> [ImageChoice] -> IO ()
setImages imgSize preferCustom imgs choices =
  zipWithM_ (setImage imgSize preferCustom) imgs (choices ++ repeat (Nothing, Nothing, Nothing))

-- | Sets an image based on the image choice (EWMHIcon, custom file, and fill color).
setImage :: Int -> Bool -> Gtk.Image -> ImageChoice -> IO ()
setImage imgSize preferCustom img imgChoice =
  case getPixBuf imgSize preferCustom imgChoice of
    Just getPixbuf -> do
      pixbuf <- getPixbuf
      scaledPixbuf <- scalePixbuf imgSize pixbuf
      Gtk.imageSetFromPixbuf img scaledPixbuf
    Nothing -> Gtk.imageClear img

-- | Get the appropriate image given an ImageChoice value
getPixBuf :: Int -> Bool -> ImageChoice -> Maybe (IO Gtk.Pixbuf)
getPixBuf imgSize preferCustom imgChoice = gpb imgChoice preferCustom
  where gpb (_, Just file, _) True = Just $ pixBufFromFile imgSize file
        gpb (Just icon, _, _) _    = Just $ pixBufFromEWMHIcon icon
        gpb (_, Just file, _) _    = Just $ pixBufFromFile imgSize file
        gpb (_, _, Just color) _   = Just $ pixBufFromColor imgSize color
        gpb _ _                    = Nothing

-- | Create a pixbuf from the pixel data in an EWMHIcon,
-- scale it square, and set it in a GTK Image.
pixBufFromEWMHIcon :: EWMHIcon -> IO Gtk.Pixbuf
pixBufFromEWMHIcon EWMHIcon {width=w, height=h, pixelsARGB=px} = do
  let pixelsPerRow = w
      bytesPerPixel = 4
      rowStride = pixelsPerRow * bytesPerPixel
      sampleBits = 8
      hasAlpha = True
      colorspace = Gtk.ColorspaceRgb
      bytesRGBA = pixelsARGBToBytesRGBA px
  cPtr <- newArray $ map CUChar bytesRGBA
  Gtk.pixbufNewFromData cPtr colorspace hasAlpha sampleBits w h rowStride

-- | Create a pixbuf from a file,
-- scale it square, and set it in a GTK Image.
pixBufFromFile :: Int -> FilePath -> IO Gtk.Pixbuf
pixBufFromFile imgSize file = Gtk.pixbufNewFromFileAtScale file imgSize imgSize False

-- | Create a pixbuf with the indicated RGBA color,
-- scale it square, and set it in a GTK Image.
pixBufFromColor :: Int -> ColorRGBA -> IO Gtk.Pixbuf
pixBufFromColor imgSize (r,g,b,a) = do
  let sampleBits = 8
      hasAlpha = True
      colorspace = Gtk.ColorspaceRgb
  pixbuf <- Gtk.pixbufNew colorspace hasAlpha sampleBits imgSize imgSize
  Gtk.pixbufFill pixbuf r g b a
  return pixbuf

-- | Take the passed in pixbuf and ensure its scaled square.
scalePixbuf :: Int -> Gtk.Pixbuf -> IO Gtk.Pixbuf
scalePixbuf imgSize pixbuf = do
  h <- Gtk.pixbufGetHeight pixbuf
  w <- Gtk.pixbufGetWidth pixbuf
  if h /= imgSize || w /= imgSize
  then
    Gtk.pixbufScaleSimple pixbuf imgSize imgSize Gtk.InterpBilinear
  else
    return pixbuf

-- | Convert a list of integer pixels to a bytestream with 4 channels.
pixelsARGBToBytesRGBA :: [Int] -> [Word8]
pixelsARGBToBytesRGBA (x:xs) = r:g:b:a:pixelsARGBToBytesRGBA xs
  where r = toByte $ x `div` 0x10000   `mod` 0x100
        g = toByte $ x `div` 0x100     `mod` 0x100
        b = toByte $ x                 `mod` 0x100
        a = toByte $ x `div` 0x1000000 `mod` 0x100
        toByte i = (fromIntegral i) :: Word8
pixelsARGBToBytesRGBA _ = []

getWindowInfoByWS :: WindowSet -> IO [[WindowInfo]]
getWindowInfoByWS windowSet = mapM (mapM getWindowInfo) x11WindowsByWorkspace
  where x11WindowsByWorkspace = map snd windowSet

getWindowInfo :: X11Window -> IO WindowInfo
getWindowInfo w = withDefaultCtx $ do
  wTitle <- getWindowTitle w
  wClass <- getWindowClass w
  wIcon <- getWindowIcons w
  return $ (wTitle, wClass, wIcon)

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

-- | Set the widget name of the frame to Workspace-<WORKSPACE_NAME>-<WORKSPACE_STATE>
setWidgetNamesByIndex :: Desktop -> String -> WorkspaceIdx -> IO ()
setWidgetNamesByIndex desktop workspaceState wsIdx =
  fromMaybe (return ()) $ setWidgetNames workspaceState <$> getWS desktop wsIdx

setWidgetNames ::  String -> Workspace -> IO ()
setWidgetNames state workspace = do
  fromMaybe (return ()) $ fmap (flip Gtk.widgetSetName widgetName) $ border workspace
  Gtk.widgetSetName (button workspace) widgetName
    where widgetName = "Workspace-" ++ (name workspace) ++ "-" ++ state

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
