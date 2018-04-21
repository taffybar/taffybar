-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Util
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------

module System.Taffybar.Util where

import           Control.Arrow ((&&&))
import           Data.Tuple.Sequence

infixl 4 ??
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab
{-# INLINE (??) #-}

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond whenTrue whenFalse =
  cond >>= (\bool -> if bool then whenTrue else whenFalse)

forkM :: Monad m => (c -> m a) -> (c -> m b) -> c -> m (a, b)
forkM a b = sequenceT . (a &&& b)

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left
