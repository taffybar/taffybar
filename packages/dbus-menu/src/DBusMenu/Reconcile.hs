module DBusMenu.Reconcile
  ( ReconcileAction (..),
    planReconciliation,
    planLabeledReconciliation,
  )
where

import Data.List (mapAccumL)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set

-- | 'ReuseItem' names the existing item to retain. With
-- 'planLabeledReconciliation' it can differ from the desired key when the
-- service renumbered an otherwise unchanged item.
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

-- | Like 'planReconciliation', but items that do not match by key then claim
-- unclaimed existing items with the same shape and label, in key order, so
-- services that renumber every item on each update (nm-applet does this
-- several times a minute) keep their widgets. No existing item is reused
-- twice.
planLabeledReconciliation ::
  (Ord key, Eq shape, Eq label) =>
  Map key (shape, label) ->
  [(key, shape, label)] ->
  [ReconcileAction key]
planLabeledReconciliation existing desired =
  snd (mapAccumL byShapeAndLabel claimedByKey (zip desired byKeyMatches))
  where
    byKeyMatches = snd (mapAccumL byKey Set.empty desired)
    byKey claimed (key, shape, _) =
      let hit =
            Set.notMember key claimed
              && (fst <$> Map.lookup key existing) == Just shape
       in if hit then (Set.insert key claimed, Just key) else (claimed, Nothing)
    claimedByKey = Set.fromList (catMaybes byKeyMatches)
    byShapeAndLabel claimed (_, Just key) = (claimed, ReuseItem key)
    byShapeAndLabel claimed ((key, shape, label), Nothing) =
      case [ k
           | (k, (s, l)) <- Map.toAscList existing,
             Set.notMember k claimed,
             s == shape,
             l == label
           ] of
        (k : _) -> (Set.insert k claimed, ReuseItem k)
        [] -> (claimed, BuildItem key)
