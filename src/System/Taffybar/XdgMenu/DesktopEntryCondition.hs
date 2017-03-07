{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.XdgMenu.DesktopEntryCondition
-- Copyright   : (c) Ulf Jasper
-- License     : GPL3 (see LICENSE)
--
-- Maintainer  : Ulf Jasper <ulf.jasper@web.de>
-- Stability   : unstable
-- Portability : unportable
--
-- See 'XdgMenuWidget' for documentation.
--
-----------------------------------------------------------------------------
module System.Taffybar.XdgMenu.DesktopEntryCondition (
  DesktopEntryCondition(..),
  matchesCondition)

where

import System.Taffybar.XdgMenu.DesktopEntry

-- | Combinable conditions for Include and Exclude statements.
data DesktopEntryCondition = Category String
                           | Filename String
                           | Not DesktopEntryCondition
                           | And [DesktopEntryCondition]
                           | Or [DesktopEntryCondition]
                           | All
                           | None
  deriving (Read, Show, Eq)

-- | Determine whether a desktop entry fulfils a condition.  
matchesCondition :: DesktopEntry -> DesktopEntryCondition -> Bool
matchesCondition de (Category cat) = deHasCategory de cat
matchesCondition de (Filename fn)  = fn == deFilename de
matchesCondition de (Not cond)     = not $ matchesCondition de cond
matchesCondition de (And conds)    = and $ map (matchesCondition de) conds
matchesCondition de (Or conds)     = or $ map (matchesCondition de) conds
matchesCondition _  All            = True
matchesCondition _  None           = False
