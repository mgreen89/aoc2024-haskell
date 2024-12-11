{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day11 (
  day11a,
)
where

-- , day11b

import AoC.Solution
import Data.Bifunctor (first)
import qualified Data.MemoTrie as Memo
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

parser :: MP.Parsec Void String [Int]
parser = MP.sepBy MPL.decimal MP.hspace

parse :: String -> Either String [Int]
parse =
  first MP.errorBundlePretty . MP.parse parser "day11"

numDigits :: Int -> Int
numDigits x = length . takeWhile (<= x) $ iterate (* 10) 1

blink :: Int -> [Int]
blink x
  | x == 0 = [1]
  | even (numDigits x) = let (a, b) = x `divMod` (10 ^ (numDigits x `div` 2)) in [a, b]
  | otherwise = [x * 2024]

steps :: Int -> Int -> Int
steps = Memo.memo2 go
 where
  go _ 0 = 1
  go x n = sum [steps y (n - 1) | y <- blink x]

day11a :: Solution [Int] Int
day11a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve =
        Right
          . sum
          . fmap (`steps` 25)
    }

day11b :: Solution _ _
day11b = Solution{sParse = Right, sShow = show, sSolve = Right}
