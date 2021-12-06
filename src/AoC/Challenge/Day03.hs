{-# LANGUAGE OverloadedStrings #-}

module AoC.Challenge.Day03
  ( day03a
  , day03b
  ) where

import           AoC.Solution
import qualified Data.ByteString               as BS
import           Data.Foldable                  ( foldl' )
import           Data.List                      ( group
                                                , sort
                                                , sortOn
                                                , transpose
                                                )
import qualified Data.Trie                     as T

binStrToInt :: String -> Int
binStrToInt = foldl' go 0
 where
  go :: Int -> Char -> Int
  go s x = (s * 2) + (if x == '1' then 1 else 0)

solvePower :: [String] -> Int
solvePower inp =
  let bits    = transpose inp
      numbits = fmap length . group . sort <$> bits
      parts   = fmap fst . sortOn snd . zip "01" <$> numbits
  in  product $ binStrToInt <$> transpose parts

day03a :: Solution [String] Int
day03a =
  Solution { sParse = Right . lines, sShow = show, sSolve = Right . solvePower }

day03b :: Solution [String] Int
day03b = Solution { sParse = Right . lines
                  , sShow  = show
                  , sSolve = Right . solveLifeSupport
                  }

binStrToBS :: String -> BS.ByteString
binStrToBS = BS.pack . fmap (\c -> if c == '1' then 1 else 0)

solveLifeSupport :: [String] -> Int
solveLifeSupport inp =
  let initial  = T.fromList $ fmap (\i -> (binStrToBS i, binStrToInt i)) inp
      o2gen    = go initial (>) ""
      co2scrub = go initial (<=) ""
  in  o2gen * co2scrub
 where
  go :: T.Trie a -> (Int -> Int -> Bool) -> BS.ByteString -> a
  go t op curr
    | T.size t == 1                  = head $ T.elems t
    | T.size zeroes == 0             = go ones op (curr `BS.snoc` 1)
    | T.size ones == 0               = go zeroes op (curr `BS.snoc` 0)
    | T.size zeroes `op` T.size ones = go zeroes op (curr `BS.snoc` 0)
    | otherwise                      = go ones op (curr `BS.snoc` 1)

   where
    zeroes = T.submap (curr `BS.snoc` 0) t
    ones   = T.submap (curr `BS.snoc` 1) t
