{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day12 (
  day12a,
)
where

--
-- , day12b

import AoC.Common.Point (Dir (..), cardinalNeighbs, dirPoint, parse2dCharMap)
import AoC.Solution
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

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

day12b :: Solution _ _
day12b = Solution{sParse = Right, sShow = show, sSolve = Right}
