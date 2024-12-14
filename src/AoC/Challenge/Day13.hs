{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day13 (
day13a,
)
where

-- 
-- , day13b

import AoC.Solution
import Data.Bifunctor (first)
import Data.Void (Void)
import Linear (V2(..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

parser :: MP.Parsec Void String [(V2 Int, V2 Int, V2 Int)]
parser =
    MP.sepBy parseMachine MP.space
  where
    parseMachine = do
        MP.string "Button A: X+"
        ax <- MPL.decimal
        MP.string ", Y+"
        ay <- MPL.decimal
        MP.newline
        MP.string "Button B: X+"
        bx <- MPL.decimal
        MP.string ", Y+"
        by <- MPL.decimal
        MP.newline
        MP.string "Prize: X="
        px <- MPL.decimal
        MP.string ", Y="
        py <- MPL.decimal
        MP.newline
        pure (V2 ax ay, V2 bx by, V2 px py)


parse :: String -> Either String [(V2 Int, V2 Int, V2 Int)]
parse =
  first MP.errorBundlePretty . MP.parse parser "day13"

day13a :: Solution _ _
day13a = Solution{sParse = Right, sShow = show, sSolve = Right}

day13b :: Solution _ _
day13b = Solution{sParse = Right, sShow = show, sSolve = Right}
