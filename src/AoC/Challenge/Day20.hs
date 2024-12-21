module AoC.Challenge.Day20 (
  day20a,
  day20b,
)
where

import AoC.Common.Graph (explore)
import AoC.Common.Point (cardinalNeighbs, manhattan, parse2dCharMap)
import AoC.Solution
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

parse :: String -> (Set (V2 Int), V2 Int, V2 Int)
parse inp =
  let raw = parse2dCharMap inp
      (_, path) = M.partition (== '#') raw
      (start, rest) = M.partition (== 'S') path
      (end, _) = M.partition (== 'E') rest
   in (M.keysSet path, fst $ M.findMin start, fst $ M.findMin end)

solve :: Int -> (Set (V2 Int), V2 Int, V2 Int) -> Int
solve maxCheat (path, start, _) = checkCheats trackList (drop 1 trackList) 0
 where
  posMap = explore legalNeighbs start
  legalNeighbs p = M.fromList [(n, 1) | n <- cardinalNeighbs p, S.member n path]
  trackList = S.toList path

  checkCheats :: [V2 Int] -> [V2 Int] -> Int -> Int
  checkCheats _ [] t = t
  checkCheats (x : xs) ys t =
    checkCheats
      xs
      (drop 1 ys)
      ( t
          + sum
            [ 1
            | y <- ys
            , manhattan x y <= maxCheat
            , Just True == do
                yScore <- posMap M.!? y
                xScore <- posMap M.!? x
                pure $ (abs (yScore - xScore) - manhattan x y) >= 100
            ]
      )
  checkCheats _ _ _ = error "Should never get here"

day20a :: Solution (Set (V2 Int), V2 Int, V2 Int) Int
day20a = Solution{sParse = Right . parse, sShow = show, sSolve = Right . solve 2}

day20b :: Solution (Set (V2 Int), V2 Int, V2 Int) Int
day20b = Solution{sParse = Right . parse, sShow = show, sSolve = Right . solve 20}
