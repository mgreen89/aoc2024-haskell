module AoC.Challenge.Day07 (
  day07a,
  day07b,
)
where

import AoC.Solution
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Functor (($>))
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

solve :: [Int -> Int -> Int] -> [(Int, NonEmpty Int)] -> Int
solve ops =
  sum . mapMaybe go
 where
  go :: (Int, NonEmpty Int) -> Maybe Int
  go (tgt, x NE.:| xs) = guard (tgt `elem` foldl' go' [x] xs) $> tgt

  go' :: [Int] -> Int -> [Int]
  go' cs x = [op a x | op <- ops, a <- cs]

day07a :: Solution [(Int, NonEmpty Int)] Int
day07a = Solution{sParse = parse, sShow = show, sSolve = Right . solve [(+), (*)]}

cat :: Int -> Int -> Int
cat x y =
  let mul = head . dropWhile (< y) $ iterate (* 10) 1
  in mul * x + y

day07b :: Solution [(Int, NonEmpty Int)] Int
day07b = Solution{sParse = parse, sShow = show, sSolve = Right . solve [(+), (*), cat]}
