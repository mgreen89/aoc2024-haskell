module AoC.Challenge.Day25 (
  day25a,
)
where

import AoC.Solution
import Data.Bifunctor (bimap)
import Data.List (transpose, partition)
import Data.List.Split (splitOn)

parse :: String -> ([[Int]], [[Int]])
parse =
    bimap (fmap fst) (fmap fst)
    . partition snd
    . fmap (handle . lines)
    . splitOn "\n\n"
 where
  handle :: [String] -> ([Int], Bool)
  handle ss =
    let isLock = head (head ss) == '#'
        toParse =
          if isLock
            then transpose ss
            else reverse <$> transpose ss
     in (,isLock) . fmap (length . takeWhile (== '#') . drop 1) $ toParse

solveA :: ([[Int]], [[Int]]) -> Int
solveA (locks, keys) =
  length
    [ ()
    | l <- locks
    , k <- keys
    , all (< 6) $ zipWith (+) k l
    ]

day25a :: Solution ([[Int]], [[Int]]) Int
day25a = Solution{sParse = Right . parse, sShow = show, sSolve = Right . solveA}
