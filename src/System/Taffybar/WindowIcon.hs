{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

-- | Helpers for resolving and constructing window icons from EWMH metadata,
-- desktop entries, icon themes, and browser tab image hooks.
module System.Taffybar.WindowIcon where

import Control.Concurrent
import qualified Control.Concurrent.MVar as MV
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader (ask, asks, runReaderT)
import Data.Bits
import Data.Data (Typeable)
import Data.Int
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.MultiMap as MM
import Data.Ord
import qualified Data.Text as T
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified GI.GdkPixbuf.Enums as Gdk
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import System.Environment.XDG.DesktopEntry
import System.Log.Logger
import System.Taffybar.Context
import System.Taffybar.Hooks
import System.Taffybar.Information.Chrome
import System.Taffybar.Information.EWMHDesktopInfo
import System.Taffybar.Information.SafeX11 (Event (PropertyEvent))
import System.Taffybar.Information.X11DesktopInfo
import System.Taffybar.Util
import System.Taffybar.Widget.Util
import Text.Printf

-- | Packed 32-bit RGBA color value used by 'pixBufFromColor'.
type ColorRGBA = Word32

data WindowIconCacheKey
  = DesktopEntryIconCacheKey Int32 String
  | ClassIconCacheKey Int32 String
  | EWMHIconCacheKey Int32 X11Window
  deriving (Eq, Ord, Show)

newtype WindowIconCache = WindowIconCache
  { windowIconCacheEntries :: MV.MVar (M.Map WindowIconCacheKey Gdk.Pixbuf)
  }
  deriving (Typeable)

getWindowIconCache :: TaffyIO WindowIconCache
getWindowIconCache = getStateDefault buildWindowIconCache

buildWindowIconCache :: TaffyIO WindowIconCache
buildWindowIconCache = do
  cacheEntries <- liftIO $ MV.newMVar M.empty
  let cache = WindowIconCache {windowIconCacheEntries = cacheEntries}
  context <- ask
  iconTheme <- liftIO Gtk.iconThemeGetDefault
  _ <- liftIO $ Gtk.onIconThemeChanged iconTheme $ do
    invalidateThemeWindowIconCacheEntries cacheEntries
    void $ runReaderT refreshTaffyWindows context
  maybeX11Context <- asks x11ContextVar
  forM_ maybeX11Context $
    const $
      void $
        subscribeToPropertyEvents [ewmhWMIcon] $ \case
          PropertyEvent _ _ _ _ windowId' _ _ _ ->
            liftIO $ invalidateEWMHWindowIconCacheEntriesForWindow cacheEntries windowId'
          _ -> return ()
  return cache

getCachedWindowIcon ::
  WindowIconCacheKey ->
  TaffyIO (Maybe Gdk.Pixbuf) ->
  TaffyIO (Maybe Gdk.Pixbuf)
getCachedWindowIcon cacheKey buildIcon = do
  WindowIconCache {windowIconCacheEntries = entriesVar} <- getWindowIconCache
  cached <- liftIO $ M.lookup cacheKey <$> MV.readMVar entriesVar
  case cached of
    Just icon -> return $ Just icon
    Nothing -> do
      icon <- buildIcon
      liftIO $
        forM_ icon $
          \resolvedIcon ->
            MV.modifyMVar_ entriesVar (return . M.insert cacheKey resolvedIcon)
      return icon

isThemeIconCacheKey :: WindowIconCacheKey -> Bool
isThemeIconCacheKey (DesktopEntryIconCacheKey _ _) = True
isThemeIconCacheKey (ClassIconCacheKey _ _) = True
isThemeIconCacheKey (EWMHIconCacheKey _ _) = False

invalidateThemeWindowIconCacheEntries ::
  MV.MVar (M.Map WindowIconCacheKey Gdk.Pixbuf) ->
  IO ()
invalidateThemeWindowIconCacheEntries entriesVar =
  MV.modifyMVar_ entriesVar $
    return . M.filterWithKey (\k _ -> not (isThemeIconCacheKey k))

invalidateEWMHWindowIconCacheEntriesForWindow ::
  MV.MVar (M.Map WindowIconCacheKey Gdk.Pixbuf) ->
  X11Window ->
  IO ()
invalidateEWMHWindowIconCacheEntriesForWindow entriesVar changedWindow =
  MV.modifyMVar_ entriesVar $
    return . M.filterWithKey keepEntry
  where
    keepEntry (EWMHIconCacheKey _ cachedWindow) _ = cachedWindow /= changedWindow
    keepEntry _ _ = True

invalidateWindowIconCacheForWindow ::
  X11Window ->
  TaffyIO ()
invalidateWindowIconCacheForWindow windowId' = do
  WindowIconCache {windowIconCacheEntries = entriesVar} <- getWindowIconCache
  liftIO $ invalidateEWMHWindowIconCacheEntriesForWindow entriesVar windowId'

-- | Convert a C array of integer pixels in the ARGB format to the ABGR format.
-- Returns an unmanged Ptr that points to a block of memory that must be freed
-- manually.
pixelsARGBToBytesABGR ::
  (Storable a, Bits a, Num a, Integral a) =>
  Ptr a -> Int -> IO (Ptr Word8)
pixelsARGBToBytesABGR ptr size = do
  target <- mallocArray (size * 4)
  let writeIndex i = do
        bits <- peekElemOff ptr i
        let b = toByte bits
            g = toByte $ bits `shift` (-8)
            r = toByte $ bits `shift` (-16)
            a = toByte $ bits `shift` (-24)
            baseTarget = 4 * i
            doPoke offset = pokeElemOff target (baseTarget + offset)
            toByte = fromIntegral . (.&. 0xFF)
        doPoke 0 r
        doPoke 1 g
        doPoke 2 b
        doPoke 3 a
      writeIndexAndNext i
        | i >= size = return ()
        | otherwise = writeIndex i >> writeIndexAndNext (i + 1)
  writeIndexAndNext 0
  return target

-- | Pick the best icon candidate for a target size from EWMH icon variants.
selectEWMHIcon :: Int32 -> [EWMHIcon] -> Maybe EWMHIcon
selectEWMHIcon imgSize icons = listToMaybe prefIcon
  where
    sortedIcons = sortBy (comparing ewmhHeight) icons
    smallestLargerIcon =
      take 1 $ dropWhile ((<= fromIntegral imgSize) . ewmhHeight) sortedIcons
    largestIcon = take 1 $ reverse sortedIcons
    prefIcon = smallestLargerIcon ++ largestIcon

-- | Create a pixbuf from the best matching EWMH icon for the requested size.
getPixbufFromEWMHIcons :: Int32 -> [EWMHIcon] -> IO (Maybe Gdk.Pixbuf)
getPixbufFromEWMHIcons size = traverse pixBufFromEWMHIcon . selectEWMHIcon size

-- | Create a pixbuf from the pixel data in an EWMHIcon.
pixBufFromEWMHIcon :: EWMHIcon -> IO Gdk.Pixbuf
pixBufFromEWMHIcon EWMHIcon {ewmhWidth = w, ewmhHeight = h, ewmhPixelsARGB = px} = do
  let width = fromIntegral w
      height = fromIntegral h
      rowStride = width * 4
  wPtr <- pixelsARGBToBytesABGR px (w * h)
  Gdk.pixbufNewFromData
    wPtr
    Gdk.ColorspaceRgb
    True
    8
    width
    height
    rowStride
    (Just free)

-- | Read EWMH icon data from an X11 window and convert it into a pixbuf.
getIconPixBufFromEWMH :: Int32 -> X11Window -> X11Property (Maybe Gdk.Pixbuf)
getIconPixBufFromEWMH size x11WindowId = runMaybeT $ do
  ewmhData <- MaybeT $ getWindowIconsData x11WindowId
  MaybeT $ lift $ withEWMHIcons ewmhData (getPixbufFromEWMHIcons size)

-- | Resolve and cache the EWMH icon for an X11 window.
getCachedIconPixBufFromEWMH :: Int32 -> X11Window -> TaffyIO (Maybe Gdk.Pixbuf)
getCachedIconPixBufFromEWMH size x11WindowId =
  getCachedWindowIcon
    (EWMHIconCacheKey size x11WindowId)
    (runX11Def Nothing $ getIconPixBufFromEWMH size x11WindowId)

-- | Create a pixbuf with the indicated RGBA color.
pixBufFromColor ::
  (MonadIO m) =>
  Int32 -> Word32 -> m Gdk.Pixbuf
pixBufFromColor imgSize c = do
  pixbuf <- fromJust <$> Gdk.pixbufNew Gdk.ColorspaceRgb True 8 imgSize imgSize
  Gdk.pixbufFill pixbuf c
  return pixbuf

-- | Resolve a desktop entry by X11 class name.
getDirectoryEntryByClass ::
  String ->
  TaffyIO (Maybe DesktopEntry)
getDirectoryEntryByClass klass = do
  entries <- MM.lookup klass <$> getDirectoryEntriesByClassName
  when (length entries > 1) $
    liftIO $
      logM "System.Taffybar.WindowIcon" DEBUG $
        printf
          "Class \"%s\" has multiple desktop entries: %s"
          klass
          (intercalate ", " $ map deFilename entries)
  return $ listToMaybe entries

-- | Try icon lookup for each parsed class component until one succeeds.
getWindowIconForAllClasses ::
  (Monad m) =>
  (p -> String -> m (Maybe a)) -> p -> String -> m (Maybe a)
getWindowIconForAllClasses doOnClass size klass =
  foldl combine (return Nothing) $ parseWindowClasses klass
  where
    combine soFar theClass =
      maybeTCombine soFar (doOnClass size theClass)

-- | Resolve a window icon through desktop-entry icon metadata.
getWindowIconFromDesktopEntryByClasses ::
  Int32 -> String -> TaffyIO (Maybe Gdk.Pixbuf)
getWindowIconFromDesktopEntryByClasses =
  getWindowIconForAllClasses getWindowIconFromDesktopEntryByClass
  where
    getWindowIconFromDesktopEntryByClass size klass =
      runMaybeT $ do
        entry <- MaybeT $ getDirectoryEntryByClass klass
        lift $
          logPrintF
            "System.Taffybar.WindowIcon"
            DEBUG
            "Using desktop entry for icon %s"
            (deFilename entry, klass)
        MaybeT $ lift $ getImageForDesktopEntry size entry

-- | Resolve and cache a window icon through desktop-entry icon metadata.
getCachedWindowIconFromDesktopEntryByClasses ::
  Int32 -> String -> TaffyIO (Maybe Gdk.Pixbuf)
getCachedWindowIconFromDesktopEntryByClasses size klass =
  getCachedWindowIcon
    (DesktopEntryIconCacheKey size klass)
    (getWindowIconFromDesktopEntryByClasses size klass)

-- | Resolve a window icon directly by class names in the icon theme.
getWindowIconFromClasses :: Int32 -> String -> IO (Maybe Gdk.Pixbuf)
getWindowIconFromClasses =
  getWindowIconForAllClasses getWindowIconFromClass
  where
    getWindowIconFromClass size klass = loadPixbufByName size (T.pack klass)

-- | Resolve and cache a window icon directly by class names in the icon theme.
getCachedWindowIconFromClasses ::
  Int32 -> String -> TaffyIO (Maybe Gdk.Pixbuf)
getCachedWindowIconFromClasses size klass =
  getCachedWindowIcon
    (ClassIconCacheKey size klass)
    (liftIO $ getWindowIconFromClasses size klass)

-- | Resolve a window icon from cached Chrome tab image data.
getPixBufFromChromeData :: X11Window -> TaffyIO (Maybe Gdk.Pixbuf)
getPixBufFromChromeData window = do
  imageData <- getChromeTabImageDataTable >>= lift . readMVar
  X11WindowToChromeTabId x11LookupMapVar <- getX11WindowToChromeTabId
  x11LookupMap <- lift $ readMVar x11LookupMapVar
  return $ tabImageData <$> (M.lookup window x11LookupMap >>= flip M.lookup imageData)
