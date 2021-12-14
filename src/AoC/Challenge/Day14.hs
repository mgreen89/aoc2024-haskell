module AoC.Challenge.Day14
  ( day14a
  , day14b
  ) where

import           AoC.Solution
import           AoC.Util                       ( listTo2Tuple )
import           Data.List.Split                ( splitOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M

parse :: String -> Either String (String, [(String, Char)])
parse inp = do
  (template, inserts) <- listTo2Tuple . splitOn "\n\n" $ inp
  insList <- traverse (listTo2Tuple . splitOn " -> ") . lines $ inserts
  pure (template, fmap (fmap head) insList)

elemFreqs :: Map String Int -> Map Char Int
elemFreqs =
  M.fromListWith (+) . concatMap (\(s, a) -> [ (c, a) | c <- s ]) . M.toList

pairCounts :: String -> Map String Int
pairCounts = M.fromListWith (+) . go []
 where
  go out []             = out
  go out [_           ] = out
  go out (a : b : rest) = go (([a, b], 1) : out) (b : rest)

insertionMap :: [(String, Char)] -> Map String [String]
insertionMap = M.fromList . fmap (\(t, c) -> (t, [[head t, c], [c, t !! 1]]))

{-
  Strategy:

  Obviously for part b, where the example has O(trillions) of elemnts,
  the naive approach of just manipulating a list of elems won't work.

  Instead, can just count all the pairs, and treat all the same pairs of
  elems in a single operation:
    - Create a frequency map of pairs in the list.
    - Each step just updates each pair AB into a pair of AX and XB, where X
      is the character to include.
    - Rinse and repeat.

  Need to be a little careful when doing the final frequency count as need to
  only count each elemnt once rather than twice (as it appears in two pairs).
-}

polyStep :: Map String [String] -> Map String Int -> Map String Int
polyStep m =
  M.fromListWith (+)
    . concatMap (\(k, a) -> [ (n, a) | n <- m M.! k ])
    . M.toList

poly :: Int -> Map String [String] -> String -> Map String Int
poly steps insMap = (!! steps) . iterate (polyStep insMap) . pairCounts

runPoly :: Int -> (String, [(String, Char)]) -> Int
runPoly steps (tmpl, ins) =
  let
    polyOut = poly steps (insertionMap ins) tmpl
    -- Be careful when counting:
    -- Will get double counts of everything except the first and last
    -- elems in the template, so add one to both of those and then
    -- halve all the counts.
    counts =
      M.elems
        . fmap (`div` 2)
        . M.unionsWith (+)
        $ [ elemFreqs polyOut
          , M.singleton (head tmpl) 1
          , M.singleton (last tmpl) 1
          ]
  in
    (maximum counts - minimum counts) `div` 2

day14a :: Solution (String, [(String, Char)]) Int
day14a = Solution { sParse = parse, sShow = show, sSolve = Right . runPoly 10 }

day14b :: Solution (String, [(String, Char)]) Int
day14b = Solution { sParse = parse, sShow = show, sSolve = Right . runPoly 40 }
