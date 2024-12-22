module AoC.Challenge.Day22 (
  day22a,
  day22b,
)
where

import AoC.Solution
import Data.Bits (shift, (.&.), (.^.))
import Data.Foldable (foldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List.Split (divvy)
import Text.Read (readEither)

evolve :: Int -> Int
evolve a =
  foldl' part a [6, -5, 11]
 where
  part x s = (x .^. (x `shift` s)) .&. 0xffffff

day22a :: Solution [Int] Int
day22a =
  Solution
    { sParse = traverse readEither . lines
    , sShow = show
    , sSolve = Right . sum . fmap ((!! 2000) . iterate evolve)
    }

solveB :: [Int] -> Int
solveB = maximum . IM.unionsWith (+) . fmap go
 where
  go :: Int -> IntMap Int
  go i =
    -- Make sure to take the first found!
    IM.fromListWith (const id) $ zip (fmap chksum seqs) (drop 4 prices)
   where
    prices = take 2001 . fmap (`mod` 10) . iterate evolve $ i
    changes = zipWith (-) (tail prices) prices
    seqs = divvy 4 1 changes

    -- Unique checksum for each sequence.  Add 9 to get all positive, then
    -- dealing with numbers in range 0-18, so use 20 as base as that's easy.
    chksum :: [Int] -> Int
    chksum = sum . zipWith (*) [1, 20, 400, 8000] . fmap (+ 9)

day22b :: Solution [Int] Int
day22b =
  Solution
    { sParse = traverse readEither . lines
    , sShow = show
    , sSolve = Right . solveB
    }
