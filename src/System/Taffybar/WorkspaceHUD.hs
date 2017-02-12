{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.WorkspaceHUD
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------

module System.Taffybar.WorkspaceHUD (
  WWC(..),
  Workspace(..),
  WorkspaceHUDConfig(..),
  WorkspaceContentsController(..),
  WorkspaceWidgetController(..),
  buildButtonController,
  buildContentsController,
  buildUnderlineButtonController,
  buildUnderlineController,
  buildWorkspaceHUD,
  buildWorkspaceWidgets,
  buildWorkspaces,
  defaultWorkspaceHUDConfig,
  getWorkspaceToWindows
) where

import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import qualified Data.Char as S
import           Data.List
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import           Data.Ord
import           Data.Word (Word8)
import           Foreign.C.Types (CUChar(..))
import           Foreign.Marshal.Array (newArray)
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Abstract.Widget as W
import qualified Graphics.UI.Gtk.Layout.Table as T
import           Graphics.X11.Xlib.Extras
import           System.Information.EWMHDesktopInfo
import           System.Taffybar.Pager
import           Text.Printf

data WorkspaceState
  = Active
  | Visible
  | Hidden
  | Empty
  | Urgent
  deriving (Show, Eq)

data IconInfo = IIEWMH EWMHIcon | IIFilePath FilePath | IINone

data Workspace =
  Workspace { workspaceIdx :: WorkspaceIdx
            , workspaceName :: String
            , workspaceState :: WorkspaceState
            , windowIds :: [X11Window]
            } deriving (Show, Eq)

class WorkspaceWidgetController wc where
  updateWidget :: wc -> Workspace -> IO wc
  getWidget :: wc -> Gtk.Widget

data WWC =
  forall a. WorkspaceWidgetController a =>
            WWC a

instance WorkspaceWidgetController WWC where
  getWidget (WWC wc) = getWidget wc
  updateWidget (WWC wc) workspace =
    WWC <$> updateWidget wc workspace

data WorkspaceContentsController = WorkspaceContentsController
  { container :: Gtk.HBox
  , label :: Gtk.Label
  , images :: [Gtk.Image]
  , contentsWorkspace :: Workspace
  , contentsConfig :: WorkspaceHUDConfig
  }

buildContentsController :: WorkspaceHUDConfig -> Workspace -> IO WWC
buildContentsController cfg ws = do
  lbl <- Gtk.labelNew (Nothing :: Maybe String)
  hbox <- Gtk.hBoxNew False 0
  Gtk.containerAdd hbox lbl
  let tempController =
        WorkspaceContentsController { container = hbox
                                    , label = lbl
                                    , images = []
                                    , contentsWorkspace =
                                      ws { windowIds = []
                                         , workspaceName = workspaceName ws ++ "fake"
                                         }
                                    , contentsConfig = cfg
                                    }
  WWC <$> updateWidget tempController ws

instance WorkspaceWidgetController WorkspaceContentsController where
  getWidget cc = Gtk.toWidget $ container cc
  updateWidget cc newWorkspace = do
    let currentWorkspace = contentsWorkspace cc

    when ((workspaceName currentWorkspace) /= (workspaceName newWorkspace)) $
         Gtk.labelSetMarkup (label cc) (workspaceName newWorkspace)

    newImages <-
      if ((windowIds currentWorkspace) /= (windowIds newWorkspace))
      then
        updateImages cc newWorkspace
      else
        return $ images cc

    Gtk.widgetSetName (container cc) $ getWidgetName newWorkspace "contents"

    return cc { contentsWorkspace = newWorkspace
              , images = newImages
              }

getIconInfo :: WorkspaceHUDConfig -> X11Window -> IO IconInfo
getIconInfo cfg w = do
  -- TODO: handle custom files
  icons <- withDefaultCtx $ getWindowIcons w
  return $ if (null icons)
           then IINone
           else IIEWMH $ selectEWMHIcon (windowIconSize cfg) icons

updateImages :: WorkspaceContentsController -> Workspace -> IO [Gtk.Image]
updateImages wcc ws = do
  iconInfos_ <- mapM (getIconInfo (contentsConfig wcc)) $ windowIds ws
  -- XXX: Only one of the two things being zipped can be an infinite list, which
  -- is why this newImagesNeeded contortion is needed.
  let iconInfos =
        if newImagesNeeded
          then iconInfos_
          else (iconInfos_ ++ repeat IINone)


  newImgs <- zipWithM setImageFromIO getImgs iconInfos
  putStrLn $
    printf "Attempt to set %s icons for %s. len newImgs %s"
      (show (length $ windowIds ws))
      (show $ workspaceIdx ws)
      (show $ length newImgs)
  when newImagesNeeded $ Gtk.widgetShowAll $ container wcc
  return newImgs
  where
    imgSize = windowIconSize $ contentsConfig wcc
    preferCustom = False
    setImageFromIO getImage iconInfo = do
      img <- getImage
      setImage imgSize preferCustom img iconInfo
      return img
    infiniteImages =
      (map return $ images wcc) ++
      (repeat $ do
         img <- Gtk.imageNew
         Gtk.containerAdd (container wcc) img
         return img)
    newImagesNeeded = (length $ images wcc) < (length $ windowIds ws)
    getImgs =
      if newImagesNeeded
        then infiniteImages
        else (map return $ images wcc)

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

-- | Sets an image based on the image choice (EWMHIcon, custom file, and fill color).
setImage :: Int -> Bool -> Gtk.Image -> IconInfo -> IO ()
setImage imgSize preferCustom img imgChoice =
  case getPixBuf imgSize preferCustom imgChoice of
    Just getPixbuf -> do
      pixbuf <- getPixbuf
      scaledPixbuf <- scalePixbuf imgSize pixbuf
      Gtk.imageSetFromPixbuf img scaledPixbuf
    Nothing -> Gtk.imageClear img

-- | Get the appropriate im\age given an ImageChoice value
getPixBuf :: Int -> Bool -> IconInfo -> Maybe (IO Gtk.Pixbuf)
getPixBuf imgSize preferCustom imgChoice = gpb imgChoice preferCustom
  where gpb (IIFilePath file) True = Just $ pixBufFromFile imgSize file
        gpb (IIEWMH icon) _ = Just $ pixBufFromEWMHIcon icon
        gpb (IIFilePath file) _ = Just $ pixBufFromFile imgSize file
        gpb _ _ = Nothing

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

-- | Convert a list of integer pixels to a bytestream with 4 channels.
pixelsARGBToBytesRGBA :: [Int] -> [Word8]
pixelsARGBToBytesRGBA (x:xs) = r:g:b:a:pixelsARGBToBytesRGBA xs
  where r = toByte $ x `div` 0x10000   `mod` 0x100
        g = toByte $ x `div` 0x100     `mod` 0x100
        b = toByte $ x                 `mod` 0x100
        a = toByte $ x `div` 0x1000000 `mod` 0x100
        toByte i = (fromIntegral i) :: Word8
pixelsARGBToBytesRGBA _ = []

-- | Create a pixbuf from a file,
-- scale it square, and set it in a GTK Image.
pixBufFromFile :: Int -> FilePath -> IO Gtk.Pixbuf
pixBufFromFile imgSize file = Gtk.pixbufNewFromFileAtScale file imgSize imgSize False

selectEWMHIcon :: Int -> [EWMHIcon] -> EWMHIcon
selectEWMHIcon imgSize icons = head prefIcon
  where sortedIcons = sortBy (comparing height) icons
        smallestLargerIcon = take 1 $ dropWhile ((<= imgSize) . height) sortedIcons
        largestIcon = take 1 $ reverse sortedIcons
        prefIcon = smallestLargerIcon ++ largestIcon

data WorkspaceHUDConfig =
  WorkspaceHUDConfig
  { widgetBuilder :: WorkspaceHUDConfig -> Workspace -> IO WWC
  , widgetGap :: Int
  , windowIconSize :: Int
  }

getWorkspaceToWindows :: IO (MM.MultiMap WorkspaceIdx X11Window)
getWorkspaceToWindows =
  withDefaultCtx getWindows >>=
  foldM
    (\theMap window ->
       MM.insert <$> withDefaultCtx (getWorkspace window)
                 <*> pure window <*> pure theMap)
    MM.empty

buildWorkspaces :: IO (M.Map WorkspaceIdx Workspace)
buildWorkspaces = do
  names <- withDefaultCtx getWorkspaceNames
  workspaceToWindows <- getWorkspaceToWindows
  active:visible <- withDefaultCtx getVisibleWorkspaces

  let getWorkspaceState idx windows
        | idx == active = Active
        | elem idx visible = Visible
        | null windows = Empty
        | otherwise = Hidden

  return $ foldl (\theMap (idx, name) ->
                    let windows = MM.lookup idx workspaceToWindows in
                    M.insert idx
                     Workspace { workspaceIdx = idx
                               , workspaceName = name
                               , workspaceState = getWorkspaceState idx windows
                               , windowIds = windows
                               } theMap) M.empty names

buildWorkspaceWidgets
  :: WorkspaceHUDConfig
  -> Gtk.HBox
  -> MV.MVar (M.Map WorkspaceIdx WWC)
  -> IO ()
buildWorkspaceWidgets cfg cont controllersRef = do
  workspacesMap <- buildWorkspaces
  let builder = (widgetBuilder cfg)
      workspaces = M.elems workspacesMap

  workspaceIDToController <-
    M.fromList <$>
    mapM (((liftM2 . liftM2) (,)) (return . workspaceIdx) $ builder cfg) workspaces

  MV.modifyMVar_ controllersRef $ const (return workspaceIDToController)

  mapM_ addWidget $ M.elems workspaceIDToController
  -- XXX: Does this belong somewhere else
  Gtk.widgetShowAll cont
    where addWidget controller =
            do
              let widget = getWidget controller
              Gtk.containerAdd cont widget
              Gtk.boxPackStart cont widget Gtk.PackNatural 0

buildWorkspaceHUD :: WorkspaceHUDConfig -> Pager -> IO Gtk.Widget
buildWorkspaceHUD cfg pager = do
  cont <- Gtk.hBoxNew False (widgetGap cfg)
  controllersRef <- MV.newMVar M.empty
  buildWorkspaceWidgets cfg cont controllersRef
  subscribe pager (onActiveChanged controllersRef) "_NET_CURRENT_DESKTOP"
  subscribe pager (onActiveChanged controllersRef) "_NET_WM_DESKTOP"
  subscribe pager (onActiveChanged controllersRef) "_NET_DESKTOP_NAMES"
  return $ Gtk.toWidget cont

  -- let cfg = config pager
  --     activecb = activeCallback cfg deskRef
  --     activefastcb = activeFastCallback cfg deskRef
  --     redrawcb = redrawCallback pager deskRef switcher
  --     urgentcb = urgentCallback cfg deskRef
  -- subscribe pager activecb "_NET_CURRENT_DESKTOP"
  -- subscribe pager activefastcb "_NET_WM_DESKTOP"
  -- subscribe pager redrawcb "_NET_DESKTOP_NAMES"
  -- subscribe pager redrawcb "_NET_NUMBER_OF_DESKTOPS"
  -- subscribe pager urgentcb "WM_HINTS"

updateAllWorkspaceWidgets :: MV.MVar (M.Map WorkspaceIdx WWC) -> IO ()
updateAllWorkspaceWidgets controllersRef = do
  workspacesMap <- buildWorkspaces
  let updateController idx controller =
        maybe (return controller) (updateWidget controller) $
        M.lookup idx workspacesMap

  MV.modifyMVar_ controllersRef $ \controllers -> do
    controllersList <-
      mapM
      (\(idx, controller) -> do
         newController <- (updateController idx controller)
         return (idx, newController)) $
      M.toList controllers
    return $ M.fromList controllersList
  return ()

onActiveChanged :: MV.MVar (M.Map WorkspaceIdx WWC) -> Event -> IO ()
onActiveChanged controllersRef _ =
  Gtk.postGUIAsync $ updateAllWorkspaceWidgets controllersRef

data WorkspaceButtonController =
  WorkspaceButtonController { button :: Gtk.EventBox
                            , buttonWorkspace :: Workspace
                            , contentsController :: WWC
                            }

instance WorkspaceWidgetController WorkspaceButtonController
  where
    getWidget wbc = Gtk.toWidget $ button wbc
    updateWidget wbc workspace = do
      newContents <- updateWidget (contentsController wbc) workspace
      updateMinSize 60 $ Gtk.toWidget $ button wbc
      return wbc { contentsController = newContents }

buildButtonController
  :: (WorkspaceHUDConfig -> Workspace -> IO WWC)
  -> WorkspaceHUDConfig
  -> Workspace
  -> IO WWC
buildButtonController contentsBuilder cfg workspace = do
  ebox <- Gtk.eventBoxNew
  cc <- contentsBuilder cfg workspace
  Gtk.containerAdd ebox $ getWidget cc
  return $ WWC WorkspaceButtonController { button = ebox
                                         , buttonWorkspace = workspace
                                         , contentsController = cc
                                         }

data UnderlineController =
  UnderlineController { table :: T.Table
                      -- XXX: An event box is used here because we need to
                      -- change the background
                      , underline :: Gtk.EventBox
                      , overlineController :: WWC
                      }

buildUnderlineController
  :: (WorkspaceHUDConfig -> Workspace -> IO WWC)
  -> WorkspaceHUDConfig
  -> Workspace
  -> IO WWC
buildUnderlineController contentsBuilder cfg workspace = do
  t <- T.tableNew 2 1 False
  u <- Gtk.eventBoxNew
  cc <- contentsBuilder cfg workspace

  -- TODO: make this size configurable
  W.widgetSetSizeRequest u (-1) 3

  T.tableAttach t (getWidget cc) 0 1 0 1 [T.Expand] [T.Expand] 0 0
  T.tableAttach t u 0 1 1 2 [T.Fill] [T.Shrink] 1 0

  return $ WWC UnderlineController { table = t
                                   , underline = u
                                   , overlineController = cc
                                   }

instance WorkspaceWidgetController UnderlineController
  where
    getWidget uc = Gtk.toWidget $ table uc
    updateWidget uc workspace = do
      Gtk.widgetSetName (underline uc) $ getWidgetName workspace "underline"
      newContents <- updateWidget (overlineController uc) workspace
      return uc { overlineController = newContents }

getWidgetName :: Workspace -> String -> String
getWidgetName ws wname =
  printf
    "Workspace-%s-%s-%s"
    wname
    (workspaceName ws)
    (map S.toLower $ show $ workspaceState ws)

buildUnderlineButtonController :: WorkspaceHUDConfig -> Workspace -> IO WWC
buildUnderlineButtonController =
  buildButtonController (buildUnderlineController buildContentsController)

defaultWorkspaceHUDConfig :: WorkspaceHUDConfig
defaultWorkspaceHUDConfig =
  WorkspaceHUDConfig { widgetBuilder = buildUnderlineButtonController
                     , widgetGap = 0
                     , windowIconSize = 16
                     }

updateMinSize :: Int -> Gtk.Widget -> IO ()
updateMinSize minWidth widget = do
  W.widgetSetSizeRequest widget (-1) (-1)
  W.Requisition w _ <- W.widgetSizeRequest widget
  when (w < minWidth) $ W.widgetSetSizeRequest widget minWidth  $ -1
