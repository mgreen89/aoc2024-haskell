module AoC.Challenge.Day13 (
  day13a,
  day13b,
)
where

import AoC.Solution
import Data.Bifunctor (first)
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Linear (V2 (..), (*^))
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
    pure (V2 ax ay, V2 bx by, V2 px py)

parse :: String -> Either String [(V2 Int, V2 Int, V2 Int)]
parse =
  first MP.errorBundlePretty . MP.parse parser "day13"

-- | Solve the simeltaneous equations and return (if possible) and integer solution.

{- Slightly simplified initial example to help work out.
94a + 22b = 840
34a + 67b = 540
a = (540 - 67b) / 34
94 * 540 / 34 - 94 * 67b / 34 + 22b = 840
94 * (540 - 67b) + (22 * 34)b = 840 * 34
((22 * 34) - (94 * 67))b = (840 * 34) - (94 * 540)
-5550b = -2220
b = 4
-}
solveSimel :: (V2 Int, V2 Int, V2 Int) -> Maybe (Int, Int)
solveSimel (V2 ax ay, V2 bx by, V2 tx ty) =
  let (b, bRem) = (tx * ay - ty * ax) `divMod` (ay * bx - ax * by)
   in if bRem == 0 then Just ((ty - (by * b)) `div` ay, b) else Nothing

day13a :: Solution [(V2 Int, V2 Int, V2 Int)] Int
day13a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . sum . fmap (\(a, b) -> 3 * a + b) . mapMaybe solveSimel
    }

check :: (V2 Int, V2 Int, V2 Int) -> (Int, Int) -> Bool
check (m, n, t) (a, b) = (a *^ m) + (b *^ n) == t

day13b :: Solution [(V2 Int, V2 Int, V2 Int)] Int
day13b =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve =
        Right
          . sum
          . fmap ((\(a, b) -> 3 * a + b) . snd)
          -- Unsure why this is required after the divMod checks?
          . filter (uncurry check)
          . mapMaybe
            ( \(m, n, t) ->
                sequenceA
                  ( (m, n, fmap (+ 10000000000000) t)
                  , solveSimel (m, n, fmap (+ 10000000000000) t)
                  )
            )
    }
