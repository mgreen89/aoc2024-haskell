module AoC.Challenge.Day06
  ( day06a
  , day06b
  ) where

import           AoC.Solution
import           Control.Monad                  ( (<=<) )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as M
import           Data.List                      ( iterate' )
import           Data.List.Split                ( splitOn )
import           Text.Read                      ( readEither )

-- Get an IntMap of integer frequency.
freqs :: [Int] -> IntMap Int
freqs = M.fromListWith (+) . fmap (, 1)

stepCount :: (Int, Int) -> [(Int, Int)]
stepCount (0, n) = [(6, n), (8, n)]
stepCount (i, n) = [(i - 1, n)]

countAfter :: Int -> [Int] -> Int
countAfter days =
  sum
    . (!! days)
    . iterate' (M.fromListWith (+) . (stepCount <=< M.toList))
    . freqs


day06a :: Solution [Int] Int
day06a = Solution { sParse = traverse readEither . splitOn ","
                  , sShow  = show
                  , sSolve = Right . countAfter 80
                  }

day06b :: Solution [Int] Int
day06b = Solution { sParse = traverse readEither . splitOn ","
                  , sShow  = show
                  , sSolve = Right . countAfter 256
                  }
