module AoC.Challenge.Day14 (
  day14a,
  day14b,
)
where

import AoC.Common.Point (allNeighbs)
import AoC.Solution
import AoC.Util (freqs)
import Data.Bifunctor (first)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Void (Void)
import Linear (V2 (..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

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
solveA w h = score w h . fmap (\(p, v) -> step w h p (100 * v))

day14a :: Solution [(V2 Int, V2 Int)] Int
day14a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA 101 103}

solveB :: Int -> Int -> [(V2 Int, V2 Int)] -> Int
solveB w h pvs =
  head
    . fmap fst
    . dropWhile (not . snd)
    . fmap (fmap anySurrounded)
    . zip [0 ..]
    $ iterate (zipWith (flip (step w h)) vs) ps
 where
  (ps, vs) = unzip pvs

  anySurrounded :: [V2 Int] -> Bool
  anySurrounded pps =
    let pSet = S.fromList pps
     in any ((`S.isSubsetOf` pSet) . S.fromList . allNeighbs) pSet

day14b :: Solution [(V2 Int, V2 Int)] Int
day14b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB 101 103}
