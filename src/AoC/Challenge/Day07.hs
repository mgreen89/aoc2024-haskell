module AoC.Challenge.Day07 (
  day07a,
  day07b,
)
where

import AoC.Solution
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

parser :: MP.Parsec Void String [(Int, NonEmpty Int)]
parser =
  MP.sepBy
    ( do
        tgt <- MPL.decimal <* MP.char ':' <* MP.hspace1
        operands <- NE.some1 (MPL.decimal <* MP.hspace)
        pure (tgt, operands)
    )
    MP.newline

parse :: String -> Either String [(Int, NonEmpty Int)]
parse =
  first MP.errorBundlePretty . MP.parse parser "day07"

solve :: [Int -> Int -> Maybe Int] -> [(Int, NonEmpty Int)] -> Int
solve ops = sum . mapMaybe (\(n, xs) -> n <$ guard (solveBack ops n xs))

solveBack :: [Int -> Int -> Maybe Int] -> Int -> NonEmpty Int -> Bool
solveBack ops tgt (x NE.:| xs) = x `elem` go tgt (reverse xs)
 where
  go a (b : bs) = [n | t <- try a b, n <- go t bs]
  go a _ = [a]

  try :: Int -> Int -> [Int]
  try a b = mapMaybe (\f -> f b a) ops

unAdd :: Int -> Int -> Maybe Int
unAdd x y = y - x <$ guard (y >= x)

unMul :: Int -> Int -> Maybe Int
unMul x y = y `div` x <$ guard (y `mod` x == 0)

day07a :: Solution [(Int, NonEmpty Int)] Int
day07a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . solve [unAdd, unMul]
    }

unCat :: Int -> Int -> Maybe Int
unCat x y =
  let mul = head . dropWhile (< x) $ iterate (* 10) 1
      (d, m) = y `divMod` mul
   in d <$ guard (m == x)

day07b :: Solution [(Int, NonEmpty Int)] Int
day07b =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . solve [unAdd, unMul, unCat]
    }
