{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module AoC.Challenge.Day08
  (
    day08a
  , day08b
  ) where

import           AoC.Solution
import Data.Char (isSpace)
import Data.List.Split (splitOn)

day08aquick :: [String] -> Int
day08aquick inp =
  let displayed = inp >>= (splitOn " " . (!! 1) . splitOn " | ")
      is1478 s = let l = length s in l < 5 || l == 7
  in
  length $ filter is1478 displayed

day08a :: Solution [String] Int
day08a = Solution { sParse = Right . lines, sShow = show, sSolve = Right . day08aquick }

day08b :: Solution _ _
day08b = Solution { sParse = Right, sShow = show, sSolve = Right }
