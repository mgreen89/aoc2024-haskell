module AoC.Challenge.Day01
  ( day01a
  , day01b
  ) where

import           AoC.Solution


countIncreases :: Int -> [Int] -> Int
countIncreases gap xs = length $ filter id $ zipWith (<) xs (drop gap xs)


day01a :: Solution [Int] Int
day01a = Solution { sParse = Right . fmap read . lines
                  , sShow  = show
                  , sSolve = Right . countIncreases 1
                  }

{-
Actually only need to count the increases of 3-apart items.
i.e. [A, B, C], D, ... and A, [B, C, D], ... only actually differ by D - A.
-}
day01b :: Solution [Int] Int
day01b = Solution { sParse = Right . fmap read . lines
                  , sShow  = show
                  , sSolve = Right . countIncreases 3
                  }
