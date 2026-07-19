module DBusMenu.Reconcile
  ( ReconcileAction (..),
    planReconciliation,
  )
where

import Data.List (mapAccumL)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data ReconcileAction key
  = ReuseItem key
  | BuildItem key
  deriving (Eq, Show)

-- | Decide which existing items can be retained in a new ordered layout.
-- Existing and desired items match by key and by shape. A duplicate desired
-- key is built rather than reusing the same widget twice.
planReconciliation :: (Ord key, Eq shape) => Map key shape -> [(key, shape)] -> [ReconcileAction key]
planReconciliation existing = snd . mapAccumL plan Set.empty
  where
    plan used (key, desiredShape) =
      let canReuse =
            Set.notMember key used
              && Map.lookup key existing == Just desiredShape
          action = if canReuse then ReuseItem key else BuildItem key
       in (Set.insert key used, action)
