module AoC.Challenge.Day01 (
  day01a,
  day01b,
)
where

import AoC.Solution
import AoC.Util (freqs)
import Data.Bifunctor (first)
import Data.List (sort)
import qualified Data.Map as M
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

parse :: String -> Either String ([Int], [Int])
parse =
  fmap unzip . first MP.errorBundlePretty . MP.parse parser "day01"

day01a :: Solution ([Int], [Int]) Int
day01a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve =
        Right
          . sum
          . fmap abs
          . (\(xs, ys) -> zipWith (-) (sort xs) (sort ys))
    }

day01b :: Solution ([Int], [Int]) Int
day01b =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve =
        Right
          . sum
          . (\(xs, ys) -> fmap (\x -> x * M.findWithDefault 0 x (freqs ys)) xs)
    }
