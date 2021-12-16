{-# LANGUAGE ScopedTypeVariables #-}

module AoC.Challenge.Day15
  ( day15a
  , day15b
  ) where

import           AoC.Solution
import           AoC.Util                       ( Point
                                                , cardinalNeighbours
                                                , maybeToEither
                                                , parseMap
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
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
  :: forall a n
   . (Ord a, Num a, Ord n)
  => Map n a     -- ^ Costs
  -> (n -> [n])  -- ^ Neighbours
  -> n           -- ^ Start
  -> n           -- ^ Destination
  -> Maybe a     -- ^ Total cost if successful
dijkstra costs getNeighbs start dest = go M.empty (PSQ.singleton start 0 0)
 where
  go :: Map n a -> OrdPSQ n a a -> Maybe a
  go visited unvisited = case M.lookup dest visited of
    Just x  -> Just x
    Nothing -> uncurry go =<< step (visited, unvisited)

  step :: (Map n a, OrdPSQ n a a) -> Maybe (Map n a, OrdPSQ n a a)
  step (v, uv) = do
    (currP, _, currV, uv') <- PSQ.minView uv
    let v' = M.insert currP currV v
    if currP == dest
    -- Short circuit if the destination has the lowest cost.
      then pure (v', uv')
      else pure (v', foldl' (handleNeighbour currV) uv' (getNeighbs currP))
   where
    handleNeighbour :: a -> OrdPSQ n a a -> n -> OrdPSQ n a a
    handleNeighbour currCost q n
      | M.member n v = q
      | otherwise = case M.lookup n costs of
        Just c  -> insertIfBetter n (c + currCost) (c + currCost) q
        Nothing -> q

walkPath :: Map Point Int -> Maybe Int
walkPath m = dijkstra m cardinalNeighbours zero (maximum (M.keys m))

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
