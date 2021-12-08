{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module AoC.Challenge.Day08
  ( day08a
  , day08b
  ) where

import           AoC.Solution
import           Data.Bifunctor                 ( first
                                                , second
                                                )
import           Data.Char                      ( isSpace )
import           Data.List                      ( foldl'
                                                , sort
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S


import           Debug.Trace

day08aquick :: [String] -> Int
day08aquick inp =
  let displayed = inp >>= (words . (!! 1) . splitOn " | ")
      is1478 s = let l = length s in l < 5 || l == 7
  in  length $ filter is1478 displayed

day08a :: Solution [String] Int
day08a = Solution { sParse = Right . lines
                  , sShow  = show
                  , sSolve = Right . day08aquick
                  }


{- Thinking for part b.

  Try and find an invariant across the normal and mixed-up wires.

  Firstly, check and see if the segments have unique counts across the digits.
  Sadly, not the case:
    {'f': 9, 'a': 8, 'c': 8, 'g': 7, 'd': 7, 'b': 6, 'e': 4}

  Okay, we have these counts, what about subbing them back into their digits?
  So 0, which was "abcefg" becomes "868497".
  Need to also then sort it for consistency so becomes "467889".
  (i.e. it contains one segment that appears 4 times across all the digits,
  two segments that appear 8 times... etc.)

  This _is_ unique! Hooray!

  Build a map of this "fingerprint" to the digit it belongs to, generate
  the fingerprints for the input data and use the map to look up the real
  digits.
-}

type Segment = Char
type Digit = Set Segment
type Fingerprint = [Int]

digits :: Map Int Digit
digits =
  M.fromList
    . zip [0 ..]
    . fmap S.fromList
    $ [ ['a', 'b', 'c', 'e', 'f', 'g']
      , ['c', 'f']
      , ['a', 'c', 'd', 'e', 'g']
      , ['a', 'c', 'd', 'f', 'g']
      , ['b', 'c', 'd', 'f']
      , ['a', 'b', 'd', 'f', 'g']
      , ['a', 'b', 'd', 'e', 'f', 'g']
      , ['a', 'c', 'f']
      , ['a', 'b', 'c', 'd', 'e', 'f', 'g']
      , ['a', 'b', 'c', 'd', 'f', 'g']
      ]

segmentCount :: [Digit] -> Map Segment Int
segmentCount = M.unionsWith (+) . fmap (M.fromList . fmap (, 1) . S.toList)

digitFingerprint :: Map Segment Int -> Digit -> Fingerprint
digitFingerprint segCounts = sort . fmap (segCounts M.!) . S.toList

-- Map of fingerprint to actual int value.
digitFingerprints :: Map Fingerprint Int
digitFingerprints =
  let segmentCounts = segmentCount $ M.elems digits
  in  M.fromList
        . flip zip [0 ..]
        . fmap (digitFingerprint segmentCounts)
        . M.elems
        $ digits

parseDisplay :: String -> (Set Digit, [Digit])
parseDisplay =
  first S.fromList
    . (\l -> (head l, head $ tail l))
    . fmap (fmap S.fromList . words)
    . splitOn " | "

findRealDigits :: Set Digit -> Map Digit Int
findRealDigits digits =
  let segCounts = segmentCount $ S.elems digits
  in  M.fromList
        . fmap (\c -> (c, digitFingerprints M.! digitFingerprint segCounts c))
        $ S.elems digits

getValue :: Map Digit Int -> [Digit] -> Int
getValue m = foldl' go 0 where go tot d = tot * 10 + (m M.! d)

getDisplayValue :: (Set Digit, [Digit]) -> Int
getDisplayValue (dgts, disp) =
  getValue (findRealDigits dgts) disp

day08b :: Solution [(Set Digit, [Digit])] Int
day08b = Solution { sParse = Right . fmap parseDisplay . lines
                  , sShow  = show
                  , sSolve = Right . sum . fmap getDisplayValue
                  }
