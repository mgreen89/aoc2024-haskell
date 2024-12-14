{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day14 (
    day14a,
)
where

-- day14a
-- , day14b

import AoC.Solution
import AoC.Util (freqs)
import Data.Bifunctor (first)
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Linear (V2(..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Debug.Trace

parser :: MP.Parsec Void String [(V2 Int, V2 Int)]
parser =
    MP.sepBy parseGuard MP.newline
  where
    parseGuard = do
        MP.string "p="
        p <- V2 <$> MPL.signed MP.hspace MPL.decimal <* MP.char ',' <*> MPL.signed MP.hspace MPL.decimal
        MP.string " v="
        v <- V2 <$> MPL.signed MP.hspace MPL.decimal <* MP.char ',' <*> MPL.signed MP.hspace MPL.decimal
        pure (p, v)


parse :: String -> Either String [(V2 Int, V2 Int)]
parse =
  first MP.errorBundlePretty . MP.parse parser "day14"


step :: Int -> Int -> V2 Int -> V2 Int -> V2 Int
step w h p v = mod <$> (p + v) <*> V2 w h

score :: Int -> Int -> [V2 Int] -> Int
score w h =
    product . freqs . mapMaybe toQuadrant
    where
        toQuadrant (V2 x y) = do
            qx <- mp $ compare x (w `div` 2)
            qy <- mp $ compare y (h `div` 2)
            pure (qx, qy)

        mp = \case
            LT -> Just False
            EQ -> Nothing
            GT -> Just True

solveA :: Int -> Int -> [(V2 Int, V2 Int)] -> Int
solveA w h  = score w h . fmap (\(p, v) -> step w h p (100 * v))

day14a :: Solution [(V2 Int, V2 Int)] Int
day14a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA 101 103}

day14b :: Solution _ _
day14b = Solution{sParse = Right, sShow = show, sSolve = Right}
