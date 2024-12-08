module AoC.Challenge.Day08 (
  day08a,
  day08b,
)
where

import AoC.Common.Point (boundingBox', inBoundingBox)

import AoC.Solution
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

parseMap :: String -> Map (V2 Int) Char
parseMap =
  M.fromList
    . concat
    . zipWith (\y -> zipWith (\x -> (V2 x y,)) [0 ..]) [0 ..]
    . lines

parse :: String -> (Map (V2 Int) Char, (V2 Int, V2 Int))
parse s =
  (M.mapMaybe f m, fromMaybe (V2 0 0, V2 0 0) (boundingBox' (M.keys m)))
 where
  m :: Map (V2 Int) Char
  m = parseMap s

  f :: Char -> Maybe Char
  f = \case
    '.' -> Nothing
    c -> Just c

-- Must be a nicer way to do this!
combin2 :: [a] -> [(a, a)]
combin2 [] = []
combin2 [_] = []
combin2 (x : ys) = [(x, y) | y <- ys] ++ combin2 ys

getAntinodesA :: (V2 Int, V2 Int) -> V2 Int -> V2 Int -> [V2 Int]
getAntinodesA bb a1 a2 =
  [ a
  | let d = a2 - a1
  , a <- [a1 + d + d, a1 - d]
  , inBoundingBox bb a
  ]

solve :: (V2 Int -> V2 Int -> [V2 Int]) -> Map (V2 Int) Char -> Int
solve getAntinodes as =
  S.size
    . M.foldl' go2 S.empty
    $ aToLocs
 where
  aToLocs :: Map Char (Set (V2 Int))
  aToLocs = M.foldlWithKey' go M.empty as

  go :: Map Char (Set (V2 Int)) -> V2 Int -> Char -> Map Char (Set (V2 Int))
  go m k v = M.insertWith S.union v (S.singleton k) m

  go2 :: Set (V2 Int) -> Set (V2 Int) -> Set (V2 Int)
  go2 antinodes antennas =
    S.union antinodes
      . S.fromList
      . concatMap (uncurry getAntinodes)
      $ combin2 (S.toList antennas)

day08a :: Solution (Map (V2 Int) Char, (V2 Int, V2 Int)) Int
day08a =
  Solution
    { sParse = Right . parse
    , sShow = show
    , sSolve = \(m, bb) -> Right $ solve (getAntinodesA bb) m
    }

getAntinodesB :: (V2 Int, V2 Int) -> V2 Int -> V2 Int -> [V2 Int]
getAntinodesB bb a1 a2 =
  takeWhile (inBoundingBox bb) (iterate (+ d) a1)
    ++ takeWhile (inBoundingBox bb) (iterate (\x -> x - d) a1)
 where
  d = a2 - a1

day08b :: Solution (Map (V2 Int) Char, (V2 Int, V2 Int)) Int
day08b =
  Solution
    { sParse = Right . parse
    , sShow = show
    , sSolve = \(m, bb) -> Right $ solve (getAntinodesB bb) m
    }
