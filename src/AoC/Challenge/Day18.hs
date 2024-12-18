module AoC.Challenge.Day18 (
  day18a,
  day18b,
)
where

import AoC.Common.Graph (aStar)
import AoC.Common.Point (cardinalNeighbs, inBoundingBox, manhattan)
import AoC.Solution
import Data.Bifunctor (first)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set)
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

findPath :: Set (V2 Int) -> Maybe Int
findPath blocks =
  aStar (manhattan end) neighbs start (== end)
 where
  start = V2 0 0
  end = V2 70 70
  bb = (start, end)
  neighbs p =
    M.fromList
      [ (n, 1)
      | n <- cardinalNeighbs p
      , inBoundingBox bb n
      , n `S.notMember` blocks
      ]

day18a :: Solution [V2 Int] Int
day18a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . fromJust . findPath . S.fromList . take 1024
    }

{- | Find the minimum entry where the predicate is satisfied, within the
given bounds.
-}
binarySearch :: (Integral a) => (a -> Bool) -> a -> a -> a
binarySearch f = go
 where
  go lower upper
    | lower == mid || upper == mid = lower + 1
    | f mid = go lower mid
    | otherwise = go mid upper
   where
    mid = lower + ((upper - lower) `div` 2)

solveB :: [V2 Int] -> V2 Int
solveB inp =
  let css = M.fromList . zip [0 ..] . scanl (flip S.insert) S.empty $ inp
   in (\i -> inp !! (i - 1))
        . binarySearch (isNothing . findPath . (css M.!)) 1025
        $ M.size css

day18b :: Solution [V2 Int] String
day18b =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . (\(V2 x y) -> show x ++ "," ++ show y) . solveB
    }
