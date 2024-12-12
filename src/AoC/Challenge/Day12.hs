module AoC.Challenge.Day12 (
  day12a,
  day12b,
)
where

import AoC.Common.Point (Dir (..), cardinalNeighbs, dirPoint, parse2dCharMap)
import AoC.Solution
import Data.Foldable (foldMap')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

-- | Find all the separate regions of points connected via cardinal moves.
cardinalRegions :: Set (V2 Int) -> [Set (V2 Int)]
cardinalRegions = start []
 where
  start :: [Set (V2 Int)] -> Set (V2 Int) -> [Set (V2 Int)]
  start seen remaining = case S.minView remaining of
    Nothing -> seen
    Just (p, ps) ->
      let (new, remaining') = fill S.empty (S.singleton p) ps
       in start (new : seen) remaining'

  fill :: Set (V2 Int) -> Set (V2 Int) -> Set (V2 Int) -> (Set (V2 Int), Set (V2 Int))
  fill old new remaining =
    if S.null new
      then (old, remaining)
      else fill old' new' remaining'
   where
    poss = S.difference (foldMap' (S.fromList . cardinalNeighbs) new) old
    new' = S.intersection poss remaining
    old' = S.union old new
    remaining' = S.difference remaining new'

toRegions :: Map (V2 Int) Char -> [Set (V2 Int)]
toRegions m =
  concat
    . M.elems
    . fmap cardinalRegions
    . M.fromListWith S.union
    $ [(x, S.singleton p) | (p, x) <- M.toList m]

getRegionDirNeighbs :: Set (V2 Int) -> Dir -> Set (V2 Int)
getRegionDirNeighbs r d = S.difference (S.map (+ dirPoint d) r) r

solve :: (Set (V2 Int) -> Int) -> Map (V2 Int) Char -> Int
solve countFences m =
  sum
    [ S.size r * countFences n
    | r <- toRegions m
    , n <- getRegionDirNeighbs r <$> [U ..]
    ]

day12a :: Solution (Map (V2 Int) Char) Int
day12a =
  Solution
    { sParse = Right . parse2dCharMap
    , sShow = show
    , sSolve = Right . solve S.size
    }

day12b :: Solution (Map (V2 Int) Char) Int
day12b =
  Solution
    { sParse = Right . parse2dCharMap
    , sShow = show
    , sSolve = Right . solve (length . cardinalRegions)
    }
