module AoC.Challenge.Day06 (
  day06a,
  day06b,
)
where

import AoC.Common.Point (
  Dir (..),
  boundingBox',
  dirPoint,
  inBoundingBox,
  parse2dCharMap,
 )
import AoC.Solution
import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

parseMap :: (Char -> Maybe a) -> String -> Map (V2 Int) a
parseMap f =
  M.mapMaybe f . parse2dCharMap

parse :: String -> (Set (V2 Int), V2 Int)
parse = bimap M.keysSet (fst . M.findMin) . M.partition id . parseMap x
 where
  x :: Char -> Maybe Bool
  x = \case
    '#' -> Just True
    '^' -> Just False
    _ -> Nothing

step :: Set (V2 Int) -> (V2 Int, Dir) -> (V2 Int, Dir)
step blocks (p, d)
  | p' `S.member` blocks = (p, d <> R)
  | otherwise = (p', d)
 where
  p' = p + dirPoint d

steps :: (V2 Int, V2 Int) -> Set (V2 Int) -> V2 Int -> [(V2 Int, Dir)]
steps bb blocks start =
  takeWhile (inBoundingBox bb . fst)
    . iterate (step blocks)
    $ (start, U)

solveA :: (Set (V2 Int), V2 Int) -> Int
solveA (blocks, startPos) =
  S.size . S.fromList . steps bb blocks $ startPos
 where
  bb = fromMaybe (V2 0 0, V2 0 0) (boundingBox' blocks)

day06a :: Solution (Set (V2 Int), V2 Int) Int
day06a = Solution{sParse = Right . parse, sShow = show, sSolve = Right . solveA}

{- | Detect loops using tortoise and hare
x is slow, y is fast.
-}
doesLoop :: (Eq a) => [a] -> Bool
doesLoop xs0 = go xs0 (drop 1 xs0)
 where
  go (x : xs) (y : _ : ys) = x == y || go xs ys
  go _ _ = False

solveB :: (Set (V2 Int), V2 Int) -> Int
solveB (blocks, startPos) =
  length
    [ p
    | p <- initPath
    , p /= startPos
    , doesLoop (steps bb (S.insert p blocks) p)
    ]
 where
  bb = fromMaybe (V2 0 0, V2 0 0) (boundingBox' blocks)

  initPath =
    S.toList
      . S.fromList
      . fmap fst
      . steps bb blocks
      $ startPos

day06b :: Solution (Set (V2 Int), V2 Int) Int
day06b = Solution{sParse = Right . parse, sShow = show, sSolve = Right . solveB}
