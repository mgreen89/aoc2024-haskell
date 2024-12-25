module AoC.Challenge.Day25 (
  day25a,
)
where

import AoC.Solution
import Data.Foldable (foldl')
import Data.List (transpose)
import Data.List.Split (splitOn)

parse :: String -> ([[Int]], [[Int]])
parse =
  foldl' (\(ls, ks) (ns, isL) -> if isL then (ns : ls, ks) else (ls, ns : ks)) ([], [])
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
     in (,isLock) . fmap (go 0 . drop 1) $ toParse
   where
    go :: Int -> String -> Int
    go n ('.' : _) = n
    go n ('#' : rest) = go (n + 1) rest
    go _ _ = error "Invalid character"

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
