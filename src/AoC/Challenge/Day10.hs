{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day10 (
  day10a,
)
where

-- , day10b

import AoC.Common.Point (cardinalNeighbs, parse2dCharMap)
import AoC.Solution
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))
import Text.Read (readMaybe)

parse :: String -> Map (V2 Int) Int
parse = M.mapMaybe (readMaybe . pure) . parse2dCharMap

solveA :: Map (V2 Int) Int -> Int
solveA topo =
  sum
    . M.elems
    . M.mapWithKey (\k _ -> bfs 0 $ S.singleton k)
    . M.filter (== 0)
    $ topo
 where
  bfs :: Int -> Set (V2 Int) -> Int
  bfs 9 curr = S.size curr
  bfs lvl curr =
    bfs (lvl + 1)
      . S.map fst
      . S.filter ((== Just (lvl + 1)) . snd)
      . S.map (\p -> (p, topo M.!? p))
      . S.fromList
      . concatMap cardinalNeighbs
      . S.toList
      $ curr

day10a :: Solution (Map (V2 Int) Int) Int
day10a = Solution{sParse = Right . parse, sShow = show, sSolve = Right . solveA}

day10b :: Solution _ _
day10b = Solution{sParse = Right, sShow = show, sSolve = Right}
