{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module AoC.Challenge.Day15
  ( day15a
  -- , day15b
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
import           Linear.Vector                  ( zero )

walkPath :: Map Point Int -> Int
walkPath m = dijkstra (M.singleton zero 0)
 where
  target :: Point
  target = maximum (M.keys m)

  dijkstra :: Map Point Int -> Int
  dijkstra pathCosts = go (pathCosts, S.empty)
   where
    go (pc, v) = case M.lookup target pc of
      Just x  -> x
      Nothing -> go (dijkstraStep (pc, v))

  dijkstraStep :: (Map Point Int, Set Point) -> (Map Point Int, Set Point)
  dijkstraStep (pc, v) =
    -- Find the node with the minimum value.
    let
      curr =
        head . sortOn snd . filter (\x -> S.notMember (fst x) v) . M.toList $ pc
    in  ( M.unionWith min pc (M.fromList . dijkstraCalc pc v $ curr)
        , S.insert (fst curr) v
        )

  dijkstraCalc :: Map Point Int -> Set Point -> (Point, Int) -> [(Point, Int)]
  dijkstraCalc c v (p, s) =
    [ (p', s + d)
    | p' <- cardinalNeighbours p
    , S.notMember p' v
    , d <- maybeToList (M.lookup p' m)
    , maybe True (> (s + d)) (M.lookup p' c)
    ]

day15a :: Solution (Map Point Int) Int
day15a =
  Solution { sParse = parseMap, sShow = show, sSolve = Right . walkPath }

day15b :: Solution _ _
day15b = Solution { sParse = Right, sShow = show, sSolve = Right }
