{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module AoC.Challenge.Day11
  ( day11a
  , day11b
  ) where

import           AoC.Solution
import           Data.Foldable                  ( toList )
import           Data.Foldable
import           Data.List                      ( unfoldr )
import           Data.List.Split
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Linear.V2                      ( V2(..) )
import           Text.Read                      ( readEither )


import           Debug.Trace

type Point = V2 Int

parseMap :: String -> Either String (Map Point Int)
parseMap = fmap createMap . traverse (traverse (readEither . pure)) . lines
 where
  createMap :: [[Int]] -> Map Point Int
  createMap = M.fromList . concat . zipWith
    (\y -> zipWith (\x -> (V2 x y :: Point, )) [0 ..])
    [0 ..]

neighbours
  :: (Traversable t, Applicative t, Num a, Num (t a), Eq (t a)) => t a -> [t a]
neighbours p =
  [ p + delta | delta <- sequence (pure [-1, 0, 1]), delta /= pure 0 ]

-- | Create a map of elem -> frequency.
getFreqs :: (Foldable f, Ord a) => f a -> Map a Int
getFreqs = M.fromListWith (+) . map (, 1) . toList

-- Rum a step.
-- This takes and generates a tuple of (flash count map, energy level map)
step :: (Map Point Int, Map Point Int) -> (Map Point Int, Map Point Int)
step (cs, m) =
  let (flashed, m') = flashAll (S.empty, fmap (+ 1) m)
      retF          = M.unionWith (+) cs (M.fromSet (const 1) flashed)
      retM          = M.union m' (M.fromSet (const 0) flashed)
  in  (retF, retM)

flashAll :: (Set Point, Map Point Int) -> (Set Point, Map Point Int)
flashAll (flashed, energy) =
  let (newFlashed, energy') = flash energy
  in  if S.null newFlashed
        then (flashed, energy')
        else flashAll (S.union flashed newFlashed, energy')


showMap :: Map Point Int -> String
showMap = unlines . chunksOf 10 . concatMap show . M.elems

-- Run all the flashes at the current state.
flash :: Map Point Int -> (Set Point, Map Point Int)
flash m =
  let (ready, notReady) = M.partition (> 9) m
      -- Increment all the neighbours of the ready ones.
      readyKeys         = M.keysSet ready
      neighbourFlashes  = getFreqs $ neighbours =<< M.keys ready
  in  ( readyKeys
      , M.restrictKeys (M.unionWith (+) m neighbourFlashes) (M.keysSet notReady)
      )

day11a :: Solution (Map Point Int) Int
day11a = Solution
  { sParse = parseMap
  , sShow  = show
  , sSolve = Right . sum . fst . (!! 100) . iterate step . (M.empty, )
  }


-- Rum a step.
-- This takes just the energy level map, and generates a tuple of
-- (flash set, energy level map) for the given step.
step' :: Map Point Int -> (Set Point, Map Point Int)
step' m =
  let (flashed, m') = flashAll (S.empty, fmap (+ 1) m)
      retM          = M.union m' (M.fromSet (const 0) flashed)
  in  (flashed, retM)

day11b :: Solution (Map Point Int) Int
day11b = Solution
  { sParse = parseMap
  , sShow  = show
  , sSolve = \m ->
               Right
                 . fst
                 . fromJust
                 . find (\(i, f) -> M.keysSet m == f)
                 . zip [1 ..]
                 . unfoldr (Just . step')
                 $ m
  }
