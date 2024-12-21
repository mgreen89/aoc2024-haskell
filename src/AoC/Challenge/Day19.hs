module AoC.Challenge.Day19 (
  day19a,
  day19b,
)
where

import AoC.Solution
import Data.Bifunctor (first)
import Data.Maybe (isJust)
import qualified Data.MemoTrie as Memo
import Data.Void (Void)
import Safe (headMay)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

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

isPossible :: [String] -> String -> Bool
isPossible parts full = isJust . headMay $ go full
 where
  go "" = [()]
  go r =
    [ x
    | p <- parts
    , let (pre, post) = splitAt (length p) r
    , pre == p
    , x <- go post
    ]

solveA :: ([String], [String]) -> Int
solveA (towels, designs) = length $ filter (isPossible towels) designs

day19a :: Solution ([String], [String]) Int
day19a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}

howManyMatches :: [String] -> String -> Int
howManyMatches parts = go
 where
  go :: String -> Int
  go = Memo.memo go2

  go2 :: String -> Int
  go2 "" = 1
  go2 r =
    sum
      [ go post
      | p <- parts
      , let (pre, post) = splitAt (length p) r
      , pre == p
      ]

solveB :: ([String], [String]) -> Int
solveB (towels, designs) =
  sum . fmap (howManyMatches towels) $ filter (isPossible towels) designs

day19b :: Solution ([String], [String]) Int
day19b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB}
