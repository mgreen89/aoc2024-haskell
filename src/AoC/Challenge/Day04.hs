module AoC.Challenge.Day04 (
  day04a,
  day04b,
)
where

import AoC.Common.Point (allDiffs, parse2dCharMap)
import AoC.Solution
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Linear (V2 (..), perp)

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
day04a =
  Solution
    { sParse = Right . parse2dCharMap
    , sShow = show
    , sSolve = Right . solveA
    }

solveB :: Map (V2 Int) Char -> Int
solveB inp =
  let
    (V2 xMin yMin) = V2 0 0
    (V2 xMax yMax) = foldl' (liftA2 max) (V2 0 0) (M.keys inp)
   in
    (`div` 2) $
      sum
        [ 1
        | x <- [xMin .. xMax]
        , y <- [yMin .. yMax]
        , let s = V2 x y
        , inp M.!? s == Just 'M'
        , d <- [V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1)]
        , inp M.!? (s + d) == Just 'A'
        , inp M.!? (s + 2 * d) == Just 'S'
        , let a = perp d
        , let b = perp . perp . perp $ d
        , ( inp M.!? (s + d + a) == Just 'M'
              && inp M.!? (s + d + b) == Just 'S'
          )
            || ( inp M.!? (s + d + a) == Just 'S'
                  && inp M.!? (s + d + b) == Just 'M'
               )
        ]

day04b :: Solution (Map (V2 Int) Char) Int
day04b =
  Solution
    { sParse = Right . parse2dCharMap
    , sShow = show
    , sSolve = Right . solveB
    }
