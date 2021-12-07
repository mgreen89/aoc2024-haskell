module AoC.Challenge.Day07
  ( day07a
  , day07b
  ) where

import           AoC.Solution
import           Data.List                      ( sort )
import           Data.List.Split                ( splitOn )
import           Text.Read                      ( readEither )

-- Calculate the sum of linear distances from the target.
calcLin :: Int -> [Int] -> Int
calcLin tgt = sum . fmap (abs . subtract tgt)

-- Get the nth triangular number
tri :: Int -> Int
tri x = x * (x + 1) `div` 2

-- Calculate the sum of triangular distances from the target.
calcTri :: Int -> [Int] -> Int
calcTri tgt = sum . fmap (tri . abs . subtract tgt)

-- Find the integer floor and ceiling of the mean.
mean :: [Int] -> [Int]
mean xs =
  let fracMean = fromIntegral (sum xs) / fromIntegral (length xs)
  in  [floor fracMean, ceiling fracMean]

-- Find the integer floor and ceiling of the median.
-- This might be a single element if they are the same.
median :: [Int] -> [Int]
median xs =
  let s      = sort xs
      l      = length xs
      (d, r) = l `divMod` 2
  in  if r == 1
        -- Odd number, exactly one median.
        then [s !! d]
        -- Even number, get the mean of the two middle entries.
        else mean . take 2 . drop (d - 1) $ s

-- Median mimizes the sum of distances.
day07a :: Solution [Int] Int
day07a = Solution
  { sParse = traverse readEither . splitOn ","
  , sShow  = show
  , sSolve = Right . (\xs -> minimum . fmap (`calcLin` xs) . median $ xs)
  }

-- The mean minimizes the sum of distance^2.
-- Not quite the same as the sum of triangluar distances (i.e. d * (d + 1) / 2))
-- but should be within 0.5 either side, so use the floor and ceil to get
-- the guess limits.
day07b :: Solution [Int] Int
day07b = Solution
  { sParse = traverse readEither . splitOn ","
  , sShow  = show
  , sSolve = Right . (\xs -> minimum . fmap (`calcTri` xs) . mean $ xs)
  }
