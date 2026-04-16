{-# LANGUAGE ImplicitPrelude #-}
module D3X.Stack
    ( -- * Stack Data Types
      StackInterval(..)
    , StackSeries(..)
      -- * Stack Layout
    , stackLayout
      -- * Helpers
    , stackYDomain
    ) where

import D3X.Prelude
import D3X.Scales (Domain(..))
import qualified Data.Map.Strict as Map
import Data.List (nub)

-- | A single stacked interval: the baseline (y0) and topline (y1).
--   @siY1 - siY0@ equals the original value for this series at this time point.
--
--   Corresponds to the @[y0, y1]@ pair in d3's stack output.
data StackInterval = StackInterval
    { siY0 :: !Double  -- ^ Lower bound (cumulative baseline)
    , siY1 :: !Double  -- ^ Upper bound (siY0 + original value)
    } deriving (Eq, Show)

-- | One series in the stacked output: a named key with one
--   'StackInterval' per time point, paired with its timestamp.
--
--   Corresponds to one element of the array returned by @d3.stack()@,
--   where each series has a @.key@ and an array of @[y0, y1]@ points.
data StackSeries t = StackSeries
    { ssKey    :: !Text               -- ^ Series label
    , ssPoints :: ![(t, StackInterval)]  -- ^ One interval per timestamp
    } deriving (Eq, Show)

-- | Compute stacked @[y0, y1]@ intervals from timestamped labeled data.
--
--   Takes data in the shape @[(timestamp, [(label, value)])]@ and produces
--   one 'StackSeries' per distinct label, where the intervals are stacked
--   so that each series sits on top of the previous one.
--
--   Series are stacked in the order they first appear in the data.
--   Missing keys at a given timestamp are treated as zero.
--
-- === d3.js equivalent
-- @
-- d3.stack()
--   .keys(["apples", "bananas", "cherries"])
--   .value(([, group], key) => group.get(key)?.quantity ?? 0)
--   (data)
-- @
--
-- === Haskell
-- @
-- stackLayout
--   [ (day1, [("apples", 3840), ("bananas", 1920)])
--   , (day2, [("apples", 1600), ("bananas", 1440)])
--   ]
-- -- [ StackSeries "apples"  [(day1, StackInterval 0 3840),    (day2, StackInterval 0 1600)]
-- -- , StackSeries "bananas" [(day1, StackInterval 3840 5760), (day2, StackInterval 1600 3040)]
-- -- ]
-- @
stackLayout :: [(t, [(Text, Double)])] -> [StackSeries t]
stackLayout [] = []
stackLayout input =
    let -- Distinct keys in first-appearance order
        keys = nub (concatMap (map fst . snd) input)

        -- For one time point, compute stacked intervals per key
        stackAt pairs =
            let valMap = Map.fromList pairs
                go (!y0, !acc) key =
                    let v  = Map.findWithDefault 0 key valMap
                        si = StackInterval y0 (y0 + v)
                    in (y0 + v, Map.insert key si acc)
            in snd (foldl' go (0, Map.empty) keys)

        -- All time points with their stacked interval maps
        stacked = [(t, stackAt pairs) | (t, pairs) <- input]

        -- Build one series by extracting its intervals across all time points
        buildSeries key = StackSeries key
            [(t, Map.findWithDefault (StackInterval 0 0) key m) | (t, m) <- stacked]

    in map buildSeries keys

-- | Compute the y-domain @(0, maxY1)@ for stacked data.
--   Feeds directly into @linearScale@ for the y-axis.
stackYDomain :: [StackSeries t] -> Domain Double
stackYDomain series =
    let maxY = maximum (0 : [siY1 si | s <- series, (_, si) <- ssPoints s])
    in Domain (0, maxY)
