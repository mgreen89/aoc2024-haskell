module AoC.Challenge.Day15
  ( day15a
  , day15b
  ) where

import           AoC.Solution
import           AoC.Util                       ( Point
                                                , cardinalNeighbours
                                                , dijkstra
                                                , maybeToEither
                                                , parseMap
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Linear.V2                      ( V2(..) )
import           Linear.Vector                  ( (*^)
                                                , zero
                                                )

walkPath :: Map Point Int -> Maybe Int
walkPath m = dijkstra getNeighbours zero (maximum (M.keys m))
 where
  getNeighbours :: Point -> Map Point Int
  getNeighbours =
    M.fromSet (m M.!)
      . S.intersection (M.keysSet m)
      . S.fromList
      . cardinalNeighbours

day15a :: Solution (Map Point Int) Int
day15a = Solution
  { sParse = parseMap
  , sShow  = show
  , sSolve = maybeToEither "Unsuccessful path finding" . walkPath
  }

walkTiled :: Map Point Int -> Maybe Int
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
  in
    walkPath tiledMap

day15b :: Solution (Map Point Int) Int
day15b = Solution
  { sParse = parseMap
  , sShow  = show
  , sSolve = maybeToEither "Unsuccessful path finding" . walkTiled
  }
