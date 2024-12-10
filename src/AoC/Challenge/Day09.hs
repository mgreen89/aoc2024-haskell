{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day09 (
  day09a,
)
where

--
-- , day09b

import AoC.Solution
import Data.Foldable (Foldable, toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Text.Read (readEither)

parse :: String -> Either String [Int]
parse = traverse (readEither . pure)

toSeq :: [Int] -> Seq (Maybe Int)
toSeq = go 0 S.empty
 where
  go n a [x] = a <> S.replicate x (Just n)
  go n a (x : s : rest) = go (n + 1) (a <> S.replicate x (Just n) <> S.replicate s Nothing) rest
  go _ _ _ = error "Invalid input"

compact :: Seq (Maybe Int) -> Seq Int
compact = go S.empty
 where
  go pref (Just x S.:<| xs) = go (pref S.|> x) xs
  go pref (Nothing S.:<| xs) = case xs of
    xs' S.:|> Nothing -> go pref (Nothing S.<| xs')
    xs' S.:|> Just a -> go (pref S.|> a) xs'
    S.Empty -> pref
  go pref S.Empty = pref

chksum :: (Foldable f) => f Int -> Int
chksum = sum . fmap (uncurry (*)) . zip [0 ..] . toList

solveA :: [Int] -> Int
solveA = chksum . compact . toSeq

day09a :: Solution [Int] Int
day09a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}

day09b :: Solution _ _
day09b = Solution{sParse = Right, sShow = show, sSolve = Right}
