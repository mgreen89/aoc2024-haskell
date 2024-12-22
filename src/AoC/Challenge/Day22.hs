{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day22 (
  day22a,
)
where

-- day22a
-- , day22b

import AoC.Solution
import Data.Bits (shift, (.&.), (.^.))
import Data.Foldable (foldl')
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

day22b :: Solution _ _
day22b = Solution{sParse = Right, sShow = show, sSolve = Right}
