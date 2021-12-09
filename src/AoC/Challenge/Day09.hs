{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module AoC.Challenge.Day09
  ( day09a
  , day09b
  ) where

import           AoC.Solution
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Linear.V2                      ( V2(..) )
import           Text.Read                      ( readEither )

type Point = V2 Int

parseMap :: String -> Either String (Map Point Int)
parseMap = fmap createMap . traverse (traverse (readEither . pure)) . lines
 where
  createMap :: [[Int]] -> Map Point Int
  createMap = M.fromList . concat . zipWith
    (\y -> zipWith (\x -> (V2 x y :: Point, )) [0 ..])
    [0 ..]

cardinalNeighbours :: [Point]
cardinalNeighbours = [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

getNeighbours :: Point -> [Point]
getNeighbours p = (+ p) <$> cardinalNeighbours

isLow :: Map Point Int -> (Point, Int) -> Bool
isLow m (p, v) = all isLowerThan (getNeighbours p)
 where
  isLowerThan q = case M.lookup q m of
    Nothing -> True
    Just v' -> v' > v

getLows :: Map Point Int -> [(Point, Int)]
getLows m = filter (isLow m) $ M.toList m

day09a :: Solution (Map Point Int) Int
day09a = Solution { sParse = parseMap
                  , sShow  = show
                  , sSolve = Right . sum . fmap ((+ 1) . snd) . getLows
                  }

day09b :: Solution _ _
day09b = Solution { sParse = Right, sShow = show, sSolve = Right }
