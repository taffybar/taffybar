{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.WindowIcon.Cache
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Shared in-memory cache for window icon pixbufs.
--
-- The cache is backend-agnostic: callers can use window identities from X11
-- or Hyprland and can also cache shared entries keyed by class/app-id/etc.
module System.Taffybar.WindowIcon.Cache
  ( WindowIconRef (..),
    IconCacheIdentity (..),
    IconCacheSource (..),
    WindowIconCacheKey (..),
    IconCacheLookup (..),
    WindowIconCache,
    getWindowIconCache,
    lookupWindowIcon,
    insertWindowIcon,
    lookupOrInsertWindowIcon,
    invalidateWindowIconKey,
    invalidateWindowIcons,
    invalidateIconsBySource,
    pruneWindowIconRefs,
    clearWindowIconCache,
    getWindowIconCacheKeys,
  )
where

import qualified Control.Concurrent.MVar as MV
import Control.Monad.Trans.Class (lift)
import Data.Int (Int32)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Information.EWMHDesktopInfo (X11Window)

-- | A backend-agnostic window reference used for window-scoped cache entries.
data WindowIconRef
  = X11WindowRef X11Window
  | HyprlandWindowRef T.Text
  deriving (Eq, Ord, Show)

-- | Identity component for cache entries.
--
-- Window-scoped entries are invalidated using 'invalidateWindowIcons' and
-- 'pruneWindowIconRefs'. Shared entries are invalidated by key/source.
data IconCacheIdentity
  = IconForWindow WindowIconRef
  | IconForClass T.Text
  | IconForAppId T.Text
  | IconForDesktopEntry T.Text
  | IconForPath FilePath
  | IconForCustom T.Text
  deriving (Eq, Ord, Show)

-- | Source category for icon data.
--
-- This is intentionally broader than current call sites so invalidation can be
-- policy-driven without changing key structure.
data IconCacheSource
  = IconSourceEWMH
  | IconSourceDesktopEntry
  | IconSourceClass
  | IconSourceChrome
  | IconSourceCustom T.Text
  deriving (Eq, Ord, Show)

-- | Full cache key.
data WindowIconCacheKey = WindowIconCacheKey
  { keySource :: IconCacheSource,
    keySize :: Int32,
    keyIdentity :: IconCacheIdentity
  }
  deriving (Eq, Ord, Show)

-- | Result of a cache lookup.
data IconCacheLookup
  = IconCacheMiss
  | IconCacheHit (Maybe Gdk.Pixbuf)

data WindowIconCacheState = WindowIconCacheState
  { entries :: M.Map WindowIconCacheKey (Maybe Gdk.Pixbuf),
    keysByWindow :: M.Map WindowIconRef (Set.Set WindowIconCacheKey)
  }

newtype WindowIconCache
  = WindowIconCache (MV.MVar WindowIconCacheState)

emptyWindowIconCacheState :: WindowIconCacheState
emptyWindowIconCacheState =
  WindowIconCacheState
    { entries = M.empty,
      keysByWindow = M.empty
    }

getWindowIconCache :: TaffyIO WindowIconCache
getWindowIconCache =
  getStateDefault $
    lift $
      WindowIconCache <$> MV.newMVar emptyWindowIconCacheState

lookupWindowRef :: WindowIconCacheKey -> Maybe WindowIconRef
lookupWindowRef WindowIconCacheKey {keyIdentity = IconForWindow ref} = Just ref
lookupWindowRef _ = Nothing

insertWindowKeyIndex ::
  WindowIconCacheKey ->
  WindowIconCacheState ->
  WindowIconCacheState
insertWindowKeyIndex key state =
  case lookupWindowRef key of
    Nothing -> state
    Just windowRef ->
      state
        { keysByWindow =
            M.alter
              (Just . maybe (Set.singleton key) (Set.insert key))
              windowRef
              (keysByWindow state)
        }

deleteWindowKeyIndex ::
  WindowIconCacheKey ->
  WindowIconCacheState ->
  WindowIconCacheState
deleteWindowKeyIndex key state =
  case lookupWindowRef key of
    Nothing -> state
    Just windowRef ->
      state
        { keysByWindow =
            M.update
              ( \windowKeys ->
                  let windowKeys' = Set.delete key windowKeys
                   in if Set.null windowKeys' then Nothing else Just windowKeys'
              )
              windowRef
              (keysByWindow state)
        }

insertEntry ::
  WindowIconCacheKey ->
  Maybe Gdk.Pixbuf ->
  WindowIconCacheState ->
  WindowIconCacheState
insertEntry key value state =
  let stateWithoutOld =
        if M.member key (entries state)
          then deleteWindowKeyIndex key state
          else state
      inserted = stateWithoutOld {entries = M.insert key value (entries stateWithoutOld)}
   in insertWindowKeyIndex key inserted

deleteEntry :: WindowIconCacheKey -> WindowIconCacheState -> WindowIconCacheState
deleteEntry key state =
  if M.member key (entries state)
    then
      deleteWindowKeyIndex key $
        state {entries = M.delete key (entries state)}
    else state

lookupWindowIcon :: WindowIconCacheKey -> TaffyIO IconCacheLookup
lookupWindowIcon key = do
  WindowIconCache cacheVar <- getWindowIconCache
  state <- lift $ MV.readMVar cacheVar
  return $ maybe IconCacheMiss IconCacheHit (M.lookup key (entries state))

insertWindowIcon :: WindowIconCacheKey -> Maybe Gdk.Pixbuf -> TaffyIO ()
insertWindowIcon key value = do
  WindowIconCache cacheVar <- getWindowIconCache
  _ <- lift $ MV.modifyMVar cacheVar $ \state -> return (insertEntry key value state, ())
  return ()

lookupOrInsertWindowIcon ::
  WindowIconCacheKey ->
  TaffyIO (Maybe Gdk.Pixbuf) ->
  TaffyIO (Maybe Gdk.Pixbuf)
lookupOrInsertWindowIcon key loadAction =
  lookupWindowIcon key >>= \case
    IconCacheHit value -> return value
    IconCacheMiss -> do
      computed <- loadAction
      WindowIconCache cacheVar <- getWindowIconCache
      lift $
        MV.modifyMVar
          cacheVar
          ( \state ->
              case M.lookup key (entries state) of
                Just existing -> return (state, existing)
                Nothing ->
                  let next = insertEntry key computed state
                   in return (next, computed)
          )

invalidateWindowIconKey :: WindowIconCacheKey -> TaffyIO ()
invalidateWindowIconKey key = do
  WindowIconCache cacheVar <- getWindowIconCache
  _ <- lift $ MV.modifyMVar cacheVar $ \state -> return (deleteEntry key state, ())
  return ()

invalidateWindowIcons :: WindowIconRef -> TaffyIO ()
invalidateWindowIcons ref = do
  WindowIconCache cacheVar <- getWindowIconCache
  _ <-
    lift $
      MV.modifyMVar cacheVar $ \state ->
        let keys = maybe [] Set.toList $ M.lookup ref (keysByWindow state)
            cleared = foldr deleteEntry state keys
         in return (cleared, ())
  return ()

invalidateIconsBySource :: IconCacheSource -> TaffyIO ()
invalidateIconsBySource source = do
  WindowIconCache cacheVar <- getWindowIconCache
  _ <-
    lift $
      MV.modifyMVar cacheVar $ \state ->
        let keysToDelete =
              M.keys $
                M.filterWithKey
                  (\WindowIconCacheKey {keySource} _ -> keySource == source)
                  (entries state)
            cleared = foldr deleteEntry state keysToDelete
         in return (cleared, ())
  return ()

pruneWindowIconRefs :: Set.Set WindowIconRef -> TaffyIO ()
pruneWindowIconRefs keepRefs = do
  WindowIconCache cacheVar <- getWindowIconCache
  _ <-
    lift $
      MV.modifyMVar cacheVar $ \state ->
        let staleRefs = filter (`Set.notMember` keepRefs) (M.keys $ keysByWindow state)
            staleKeys =
              concatMap
                ( \ref ->
                    maybe [] Set.toList $
                      M.lookup ref (keysByWindow state)
                )
                staleRefs
            cleared = foldr deleteEntry state staleKeys
         in return (cleared, ())
  return ()

clearWindowIconCache :: TaffyIO ()
clearWindowIconCache = do
  WindowIconCache cacheVar <- getWindowIconCache
  _ <- lift $ MV.modifyMVar cacheVar $ const $ return (emptyWindowIconCacheState, ())
  return ()

getWindowIconCacheKeys :: TaffyIO [WindowIconCacheKey]
getWindowIconCacheKeys = do
  WindowIconCache cacheVar <- getWindowIconCache
  state <- lift $ MV.readMVar cacheVar
  return $ M.keys $ entries state
