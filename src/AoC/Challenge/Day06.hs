{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day06 (
  day06a,
)
where

--
-- , day06b

import AoC.Common.Point (Dir (..), boundingBox', dirPoint, dirRot, inBoundingBox)
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
  M.mapMaybe f
    . M.fromList
    . concat
    . zipWith (\y -> zipWith (\x -> (V2 x y,)) [0 ..]) [0 ..]
    . lines

parse :: String -> (Set (V2 Int), V2 Int)
parse = bimap M.keysSet (fst . M.findMin) . M.partition id . parseMap x
 where
  x :: Char -> Maybe Bool
  x = \case
    '#' -> Just True
    '^' -> Just False
    _ -> Nothing

step :: Set (V2 Int) -> (V2 Int, Dir) -> (V2 Int, Dir)
step blocks (p, d) =
  if (p + dirPoint d) `S.member` blocks
    then (p, dirRot d R)
    else (p + dirPoint d, d)

solveA :: (Set (V2 Int), V2 Int) -> Int
solveA (blocks, startPos) =
  S.size
    . S.fromList
    . takeWhile (inBoundingBox bb)
    . fmap fst
    . iterate (step blocks)
    $ (startPos, U)
 where
  bb = fromMaybe (V2 0 0, V2 0 0) (boundingBox' blocks)

day06a :: Solution (Set (V2 Int), V2 Int) Int
day06a = Solution{sParse = Right . parse, sShow = show, sSolve = Right . solveA}

day06b :: Solution _ _
day06b = Solution{sParse = Right, sShow = show, sSolve = Right}
