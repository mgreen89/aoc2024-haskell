{-# LANGUAGE ScopedTypeVariables #-}

module AoC.Challenge.Day15
  ( day15a
  , day15b
  ) where

import           AoC.Solution
import           AoC.Util                       ( Point
                                                , cardinalNeighbours
                                                , parseMap
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.List                      ( sortOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , maybeToList
                                                )
import           Data.OrdPSQ                    ( OrdPSQ )
import qualified Data.OrdPSQ                   as PSQ
import           Linear.V2                      ( V2(..) )
import           Linear.Vector                  ( (*^)
                                                , zero
                                                )

insertIfBetter :: (Ord k, Ord p) => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
insertIfBetter k p x q = case PSQ.lookup k q of
  Nothing -> PSQ.insert k p x q
  Just (p', _) | p < p'    -> PSQ.insert k p x q
               | otherwise -> q

dijkstra
  :: forall a
   . (Ord a, Num a)
  => Map Point a -- ^ Costs
  -> Point       -- ^ Start
  -> Point       -- ^ Destination
  -> a           -- ^ Total cost
dijkstra costs start dest = go M.empty (PSQ.singleton start 0 0)
 where
  go :: Map Point a -> OrdPSQ Point a a -> a
  go visited unvisited = case M.lookup dest visited of
    Just x  -> x
    Nothing -> uncurry go $ step (visited, unvisited)

  step :: (Map Point a, OrdPSQ Point a a) -> (Map Point a, OrdPSQ Point a a)
  step (v, uv) =
    let (currP, _, currV, uv') = fromJust $ PSQ.minView uv
        v'                     = M.insert currP currV v
    in 
      -- Short circuit if the dest is the lowest unvisited.
        if currP == dest
          then (v', uv')
          else
            ( v'
            , foldl' (\psq (n, v) -> PSQ.insert n v v psq)
                     uv'
                     (calcNeighbs (currP, currV))
            )
   where
    calcNeighbs :: (Point, a) -> [(Point, a)]
    calcNeighbs (p, c) =
      [ (p', c + d)
      | p' <- cardinalNeighbours p
-- Only check neigbours that are not visited
      , M.notMember p' v
-- Only check neighbours that exist!
      , d <- maybeToList (M.lookup p' costs)
-- Only consider neighbours where the new path is lower cost.
      , maybe True (\(_, v) -> c + d < v) (PSQ.lookup p' uv)
      ]

walkPath :: Map Point Int -> Int
walkPath m = dijkstra m zero (maximum (M.keys m))

day15a :: Solution (Map Point Int) Int
day15a =
  Solution { sParse = parseMap, sShow = show, sSolve = Right . walkPath }

walkTiled :: Map Point Int -> Int
walkTiled m =
  let
    -- Add one to get the side lengths as the points are zero-indexed.
    (V2 xMax yMax) = V2 1 1 + maximum (M.keys m)
    incX           = V2 xMax 0
    incY           = V2 0 yMax

    tiledMap :: Map Point Int
    tiledMap = M.unions
      [ M.fromList
        . fmap
            (\(k, a) ->
              (k + (i *^ incX) + (j *^ incY), ((a - 1 + i + j) `mod` 9) + 1)
            )
        . M.toList
        $ m
      | i <- [0 .. 4]
      , j <- [0 .. 4]
      ]

    target = maximum (M.keys tiledMap)
  in
    dijkstra tiledMap zero target

day15b :: Solution (Map Point Int) Int
day15b =
  Solution { sParse = parseMap, sShow = show, sSolve = Right . walkTiled }
