module AoC.Common (
  (!?),
  windows,
  pairs,
  combinations,
  listTup2,
  listTup3,
) where

import Data.Foldable (foldl')
import Data.List (tails)

-- Safe, strict, list index
(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x : _) !? 0 = Just x
(x : xs) !? n = x `seq` (xs !? (n - 1))

-- | Sliding windows over a list.
windows :: Int -> [a] -> [[a]]
windows n = foldr (zipWith (:)) (repeat []) . take n . tails

-- | Sliding pairs over a list.
pairs :: [a] -> [(a, a)]
pairs (x : y : z) = (x, y) : pairs (y : z)
pairs _ = []

-- | Get all 2-combinations of list elements.
combinations :: [a] -> [(a, a)]
combinations = reverse . go []
 where
  go :: [(a, a)] -> [a] -> [(a, a)]
  go acc [] = acc
  go acc [_] = acc
  go acc (x : ys) = go (foldl' (\a y -> (x, y) : a) acc ys) ys

-- | Safely convert a list to a 2-tuple of the elements.
listTup2 :: [a] -> Maybe (a, a)
listTup2 [a, b] = Just (a, b)
listTup2 _ = Nothing

-- | Safely convert a list to a 3-tuple of the elements.
listTup3 :: [a] -> Maybe (a, a, a)
listTup3 [a, b, c] = Just (a, b, c)
listTup3 _ = Nothing
