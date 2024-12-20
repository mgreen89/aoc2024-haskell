{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day19 (
  day19a,
)
where

-- day19a
-- , day19b

import AoC.Solution
import Data.Bifunctor (first)
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Safe (headMay)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

parser :: MP.Parsec Void String ([String], [String])
parser = do
  towels <- MP.sepBy (MP.many MP.lowerChar) (MP.string ", ")
  MP.newline
  MP.newline
  designs <- MP.sepBy (MP.many MP.lowerChar) MP.newline
  pure (towels, designs)

parse :: String -> Either String ([String], [String])
parse =
  first MP.errorBundlePretty . MP.parse parser "day19"

solveA :: ([String], [String]) -> Int
solveA (towels, designs) =
  length $ mapMaybe (headMay . go) designs
 where
  go "" = [()]
  go r =
    [ x
    | t <- towels
    , take (length t) r == t
    , x <- go (drop (length t) r)
    ]

day19a :: Solution ([String], [String]) Int
day19a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}

day19b :: Solution _ _
day19b = Solution{sParse = Right, sShow = show, sSolve = Right}
