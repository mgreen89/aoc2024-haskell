module AoC.Challenge.Day07
  ( day07a
  , day07b
  ) where

import           AoC.Solution
import           Data.List.Split                ( splitOn )
import           Text.Read                      ( readEither )

calc :: (Int -> Int -> Int) -> [Int] -> Int
calc f xs =
  let minx = minimum xs
      maxx = maximum xs
      calc tgt = sum $ fmap (f tgt) xs
  in  minimum $ [ calc tgt | tgt <- [minx .. maxx] ]

day07a :: Solution [Int] Int
day07a = Solution { sParse = traverse readEither . splitOn ","
                  , sShow  = show
                  , sSolve = Right . calc (\tgt -> abs . subtract tgt)
                  }

-- Get the nth triangular number
tri :: Int -> Int
tri x = x * (x + 1) `div` 2

day07b :: Solution [Int] Int
day07b = Solution { sParse = traverse readEither . splitOn ","
                  , sShow  = show
                  , sSolve = Right . calc (\tgt x -> tri . abs $ tgt - x)
                  }
