{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module AoC.Challenge.Day03
  ( day03a
  , day03b
  ) where

import           AoC.Solution
import           Data.Foldable
import           Data.List


binStrToInt :: String -> Int
binStrToInt = foldl' go 0
 where
  go :: Int -> Char -> Int
  go s x = (s * 2) + (if x == '1' then 1 else 0)

solvePower :: [String] -> Int
solvePower inp =
  let bits    = transpose inp
      numbits = fmap length . group . sort <$> bits
      parts   = fmap fst . sortOn snd . zip "01" <$> numbits
  in  product $ binStrToInt <$> transpose parts

day03a :: Solution [String] Int
day03a =
  Solution { sParse = Right . lines, sShow = show, sSolve = Right . solvePower }

day03b :: Solution _ _
day03b = Solution { sParse = Right, sShow = show, sSolve = Right }
