{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day01 (day01a
  )
where


-- , day01b

import AoC.Solution
import Data.Bifunctor (first)
import Data.List (sort)
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

parser :: MP.Parsec Void String [(Int, Int)]
parser =
  flip MP.sepBy (MP.char '\n') $ do
    a <- MPL.decimal
    MP.space
    b <- MPL.decimal
    pure (a, b)

day01a :: Solution [(Int, Int)] Int
day01a = Solution{sParse = first MP.errorBundlePretty . MP.parse parser "day01",
       sShow = show,
       sSolve = Right . sum . fmap abs . (\(xs, ys) -> zipWith (-) (sort xs) (sort ys)) . unzip
}

day01b :: Solution _ _
day01b = Solution{sParse = Right, sShow = show, sSolve = Right}
