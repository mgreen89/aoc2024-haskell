module AoC.Challenge.Day07
  ( day07a
  , day07b
  ) where

import           AoC.Solution
import           Data.List                      ( sort )
import           Data.List.Split                ( splitOn )
import           Text.Read                      ( readEither )

calc :: Int -> Int -> (Int -> Int -> Int) -> [Int] -> Int
calc minx maxx f xs =
  let go tgt = sum $ fmap (f tgt) xs
  in  minimum $ [ go tgt | tgt <- [minx .. maxx] ]

-- Uses the refine function to generate a (min, max) bound to calculate
-- using the given calculation function.
calcReduced
  :: ([Int] -> (Int, Int))  -- refine function
  -> (Int -> Int -> Int)    -- calculate function
  -> [Int]                  -- input list
  -> Int
calcReduced refine f xs = uncurry calc (refine xs) f xs

mean :: [Int] -> (Int, Int)
mean xs =
  let fracMean = fromIntegral (sum xs) / fromIntegral (length xs)
  in  (floor fracMean, ceiling fracMean)

median :: [Int] -> (Int, Int)
median xs =
  let s      = sort xs
      l      = length xs
      (d, r) = l `divMod` 2
  in  if r == 1
        -- Odd number, exactly one median.
        then (s !! d, s !! d)
        -- Even number, return the mean (floor, ceil) of the middle entries.
        else mean . take 2 . drop (d - 1) $ s

-- Median mimizes the sum of distances.
day07a :: Solution [Int] Int
day07a = Solution
  { sParse = traverse readEither . splitOn ","
  , sShow  = show
  , sSolve = Right . calcReduced median (\tgt -> abs . subtract tgt)
  }

-- Get the nth triangular number
tri :: Int -> Int
tri x = x * (x + 1) `div` 2

-- The mean minimizes the sum of distance^2.
-- Not quite the same as the sum of triangluar distances (i.e. d * (d + 1) / 2))
-- but should be within 0.5 either side, so use the floor and ceil to get
-- the guess limits.
day07b :: Solution [Int] Int
day07b = Solution
  { sParse = traverse readEither . splitOn ","
  , sShow  = show
  , sSolve = Right . calcReduced mean (\tgt x -> tri . abs $ tgt - x)
  }
