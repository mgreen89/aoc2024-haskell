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
import           Data.Foldable                  ( minimumBy )
import           Data.List                      ( sortOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( maybeToList )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Linear.V2                      ( V2(..) )
import           Linear.Vector                  ( (*^)
                                                , zero
                                                )

import           Data.Foldable                  ( for_ )
import           Data.Maybe
import           Debug.Trace
import           System.IO.Unsafe
import Text.Printf

dijkstra
  :: forall a
   . (Ord a, Num a)
  => Map Point a -- ^ Costs
  -> Point       -- ^ Start
  -> Point       -- ^ Destination
  -> (a, Map Point a)           -- ^ Final cost
dijkstra costs start dest = go M.empty (M.singleton start 0)
 where
  go :: Map Point a -> Map Point a -> (a, Map Point a)
  go visited unvisited = case M.lookup dest visited of
    Just x  -> (x, visited)
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
walkPath m = fst $ dijkstra m zero (maximum (M.keys m))

day15a :: Solution (Map Point Int) Int
day15a =
  Solution { sParse = parseMap, sShow = show, sSolve = Right . walkPath }

walkTiled :: Map Point Int -> Int
walkTiled m =
  let
    xMax = maximum . fmap (\(V2 x _) -> x) $ M.keys m
    yMax = maximum . fmap (\(V2 _ y) -> y) $ M.keys m
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

    (r, v) = dijkstra (printMap tiledMap) zero ((incX + incY) * 5)
  in
    printMap v M.! ((5 *^ incX) + (5 *^ incY))

printMap :: Map Point Int -> Map Point Int
printMap m =
  let xMax = maximum . fmap (\(V2 x _) -> x) $ M.keys m
      yMax = maximum . fmap (\(V2 _ y) -> y) $ M.keys m
  in  unsafePerformIO $ do
        for_
          [0 .. yMax]
          (\y -> do
            for_ [0 .. xMax] (\x -> printf "%d" (m M.! V2 x y))
            putStr "\n"
          )
        pure m

day15b :: Solution (Map Point Int) Int
day15b =
  Solution { sParse = parseMap, sShow = show, sSolve = Right . walkTiled }
