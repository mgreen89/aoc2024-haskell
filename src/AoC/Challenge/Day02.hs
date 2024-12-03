module AoC.Challenge.Day02 (
  day02a,
  day02b,
)
where

import AoC.Solution
import Data.Bifunctor (first)
import Data.Ix (inRange)
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

parser :: MP.Parsec Void String [[Int]]
parser =
  flip MP.sepBy (MP.char '\n') $ do
    MP.many (MPL.decimal <* MP.hspace)

parse :: String -> Either String [[Int]]
parse =
  first MP.errorBundlePretty . MP.parse parser "day02"

dropEach :: [a] -> [[a]]
dropEach [] = [[]]
dropEach (x : xs) = xs : fmap (x :) (dropEach xs)

safe :: [Int] -> Bool
safe xs =
  any (all (inRange (1, 3))) [steps, fmap negate steps]
 where
  steps = zipWith (-) xs (drop 1 xs)

day02a :: Solution [[Int]] Int
day02a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . length . filter id . fmap safe
    }

day02b :: Solution [[Int]] Int
day02b =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . length . filter id . fmap (any safe . dropEach)
    }
