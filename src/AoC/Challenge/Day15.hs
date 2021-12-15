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
import           Data.List                      ( sortOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( maybeToList )
import           Linear.V2                      ( V2(..) )
import           Linear.Vector                  ( (*^)
                                                , zero
                                                )

dijkstra
  :: forall a
   . (Ord a, Num a)
  => Map Point a -- ^ Costs
  -> Point       -- ^ Start
  -> Point       -- ^ Destination
  -> a           -- ^ Total cost
dijkstra costs start dest = go M.empty (M.singleton start 0)
 where
  go :: Map Point a -> Map Point a -> a
  go visited unvisited = case M.lookup dest visited of
    Just x  -> x
    Nothing -> uncurry go $ step (visited, unvisited)

  step :: (Map Point a, Map Point a) -> (Map Point a, Map Point a)
  step (v, uv) =
    let curr@(currP, currV) = head . sortOn snd . M.toList $ uv
        v'                  = M.insert currP currV v
        uv'                 = M.delete currP uv
    in 
      -- Short circuit if the dest is the lowest unvisited.
        if currP == dest
          then (v', uv')
          else (v', M.unionWith min uv' (M.fromList (calcNeighbs curr)))
   where
    calcNeighbs :: (Point, a) -> [(Point, a)]
    calcNeighbs (p, c) =
      [ (p', c + d)
      | p' <- cardinalNeighbours p
-- Only check neigbours that are not visited
      , M.notMember p' v
-- Only check neighbours that exist!
      , d <- maybeToList (M.lookup p' costs)
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
    incX = V2 xMax 0
    incY = V2 0 yMax

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
