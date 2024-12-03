{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day03 (
  day03a,
)
where

-- day03a
-- , day03b

import AoC.Solution
import Data.Bifunctor (first)
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

parser :: MP.Parsec Void String [Int]
parser =
  MP.some (MP.try (MP.skipManyTill MP.anySingle (MP.try mul)))
 where
  mul :: MP.Parsec Void String Int
  mul = MP.between (MP.string "mul(") (MP.char ')') $ do
    a <- MPL.decimal
    MP.char ','
    b <- MPL.decimal
    pure $ a * b

parse :: String -> Either String [Int]
parse =
  first MP.errorBundlePretty . MP.parse parser "day03"

day03a :: Solution [Int] Int
day03a = Solution{sParse = parse, sShow = show, sSolve = Right . sum}

day03b :: Solution _ _
day03b = Solution{sParse = Right, sShow = show, sSolve = Right}
