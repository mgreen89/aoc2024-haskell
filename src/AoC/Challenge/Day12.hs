module AoC.Challenge.Day12 (
  day12a,
  day12b,
)
where

import AoC.Common.Point (Dir (..), cardinalNeighbs, dirPoint, parse2dCharMap)
import AoC.Solution
import Data.Foldable (foldMap')
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

findRegions :: Set (V2 Int) -> [Set (V2 Int)]
findRegions = start []
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


findRegion :: (Eq a) => Map (V2 Int) a -> a -> V2 Int -> Set (V2 Int)
findRegion m x p = go S.empty [p]
 where
  go :: Set (V2 Int) -> [V2 Int] -> Set (V2 Int)
  go seen [] = seen
  go seen prev =
    let seen' = S.union seen (S.fromList prev)
     in go
          seen'
          ( nub
              [ n
              | pr <- prev
              , n <- cardinalNeighbs pr
              , m M.!? n == Just x
              , not (S.member n seen')
              ]
          )

toRegions :: Map (V2 Int) Char -> [Set (V2 Int)]
toRegions m = go [] m
 where
  go a remain =
    case M.lookupMin remain of
      Nothing -> a
      Just (p, c) ->
        let region = findRegion m c p
         in go (region : a) (M.withoutKeys remain region)

getRegionDirNeighbs :: Set (V2 Int) -> Dir -> Set (V2 Int)
getRegionDirNeighbs r d = S.difference (S.map (+ dirPoint d) r) r

solveA :: Map (V2 Int) Char -> Int
solveA m =
  sum
    [ S.size r * S.size n
    | r <- toRegions m
    , n <- getRegionDirNeighbs r <$> [U ..]
    ]

day12a :: Solution (Map (V2 Int) Char) Int
day12a =
  Solution
    { sParse = Right . parse2dCharMap
    , sShow = show
    , sSolve = Right . solveA
    }

solveB :: Map (V2 Int) Char -> Int
solveB m =
  sum
    [ S.size r * length (findRegions rdns)
    | r <- toRegions m
    , d <- [U ..]
    , let rdns = getRegionDirNeighbs r d
    ]

day12b :: Solution (Map (V2 Int) Char) Int
day12b =
  Solution
    { sParse = Right . parse2dCharMap
    , sShow = show
    , sSolve = Right . solveB
    }
