{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day18 (
    day18a
)
where

-- day18a
-- , day18b

import AoC.Common.Graph (aStar)
import AoC.Common.Point (cardinalNeighbs, inBoundingBox, manhattan)
import AoC.Solution
import Data.Bifunctor (first)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Void (Void)
import Linear (V2 (..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

parser :: MP.Parsec Void String [V2 Int]
parser =
  MP.sepBy (V2 <$> MPL.decimal <* MP.char ',' <*> MPL.decimal) MP.newline

parse :: String -> Either String [V2 Int]
parse =
  first MP.errorBundlePretty . MP.parse parser "day18"

solveA :: [V2 Int] -> Int
solveA inp =
  let start = V2 0 0
      end = V2 70 70
      bb = (start, end)
      corrupted = S.fromList $ take 1024 inp
      neighbs p =
        M.fromList
          [ (n, 1)
          | n <- cardinalNeighbs p
          , inBoundingBox bb n
          , n `S.notMember` corrupted
          ]
   in fromJust $ aStar (manhattan end) neighbs start (== end)

day18a :: Solution [V2 Int] _
day18a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}

day18b :: Solution _ _
day18b = Solution{sParse = Right, sShow = show, sSolve = Right}
