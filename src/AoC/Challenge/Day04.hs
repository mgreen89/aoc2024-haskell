{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day04 (
  day04a,
)
where

-- , day04b

import AoC.Common.Point (allDiffs)
import AoC.Solution
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Linear (V2 (..))

-- | Parse String data into a Map
parse2dMap :: String -> Map (V2 Int) Char
parse2dMap = createMap . lines
 where
  createMap :: [String] -> Map (V2 Int) Char
  createMap =
    M.fromList
      . concat
      . zipWith
        (\y -> zipWith (\x -> (V2 x y,)) [0 ..])
        [0 ..]

solveA :: Map (V2 Int) Char -> Int
solveA inp =
  let
    (V2 xMin yMin) = V2 0 0
    (V2 xMax yMax) = foldl' (liftA2 max) (V2 0 0) (M.keys inp)
   in
    sum
      [ 1
      | x <- [xMin .. xMax]
      , y <- [yMin .. yMax]
      , let s = V2 x y
      , inp M.!? s == Just 'X'
      , d <- allDiffs
      , inp M.!? (s + d) == Just 'M'
      , inp M.!? (s + 2 * d) == Just 'A'
      , inp M.!? (s + 3 * d) == Just 'S'
      ]

day04a :: Solution (Map (V2 Int) Char) Int
day04a = Solution{sParse = Right . parse2dMap, sShow = show, sSolve = Right . solveA}

day04b :: Solution _ _
day04b = Solution{sParse = Right, sShow = show, sSolve = Right}
