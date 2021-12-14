module AoC.Challenge.Day14
  ( day14a
  , day14b
  ) where

import           AoC.Solution
import           AoC.Util                       ( freqs
                                                , listTo2Tuple
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.List.Split                ( splitOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M

parse :: String -> Either String (String, [(String, Char)])
parse inp = do
  (template, inserts) <- listTo2Tuple . splitOn "\n\n" $ inp
  insList <- traverse (listTo2Tuple . splitOn " -> ") . lines $ inserts
  pure (template, fmap (fmap head) insList)

polyStep :: Map String Char -> String -> String
polyStep m = reverse . go []
 where
  go :: String -> String -> String
  go out []             = out
  go out [a           ] = a : out
  go out (a : b : rest) = go (m M.! [a, b] : a : out) (b : rest)

-- Naively expand the list
partA :: (String, [(String, Char)]) -> Int
partA (tmpl, ins) =
  let insMap     = M.fromList ins
      fullChain  = (!! 10) . iterate (polyStep insMap) $ tmpl
      fullCounts = freqs fullChain
  in  maximum (M.elems fullCounts) - minimum (M.elems fullCounts)

day14a :: Solution (String, [(String, Char)]) Int
day14a = Solution { sParse = parse, sShow = show, sSolve = Right . partA }

-- Create a frequence map of pairs in the list.
-- Then a step just updates each pair AB into a pair of AX and XB, where X
-- is the character ot include.

polyStep' :: Map String [String] -> Map String Int -> Map String Int
polyStep' m =
  M.unionsWith (+)
    . fmap (\(k, a) -> M.fromList [ (n, a) | n <- m M.! k ])
    . M.toList

freqs' :: Map String Int -> Map Char Int
freqs' =
  M.unionsWith (+)
    . fmap (\(s, a) -> M.fromListWith (+) [ (c, a) | c <- s ])
    . M.toList

pairCounts :: String -> Map String Int
pairCounts = M.fromListWith (+) . go []
 where
  go out []             = out
  go out [a           ] = out
  go out (a : b : rest) = go (([a, b], 1) : out) (b : rest)

partB :: (String, [(String, Char)]) -> Int
partB (tmpl, ins) =
  let insMap :: Map String [String]
      insMap =
        M.fromList . fmap (\(t, c) -> (t, [[head t, c], [c, t !! 1]])) $ ins
      initCounts       = pairCounts tmpl
      finishCounts     = (!! 40) . iterate (polyStep' insMap) $ initCounts
      -- This has double counts of everything except the first and last chars.
      -- Fix it!
      charCounts       = freqs' finishCounts
      acutalCharCounts = M.unionsWith
        (+)
        [charCounts, M.singleton (head tmpl) 1, M.singleton (last tmpl) 1]
      counts = M.elems acutalCharCounts
  in  (maximum counts - minimum counts) `div` 2

day14b :: Solution (String, [(String, Char)]) Int
day14b = Solution { sParse = parse, sShow = show, sSolve = Right . partB }
