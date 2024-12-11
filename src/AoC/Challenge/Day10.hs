module AoC.Challenge.Day10 (
  day10a,
  day10b,
)
where

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

solveB :: Map (V2 Int) Int -> Int
solveB topo =
  sum
    . M.elems
    . M.mapWithKey (\k _ -> bfs 0 $ [k])
    . M.filter (== 0)
    $ topo
 where
  bfs :: Int -> [V2 Int] -> Int
  bfs 9 curr = length curr
  bfs lvl curr =
    bfs (lvl + 1)
      . fmap fst
      . filter ((== Just (lvl + 1)) . snd)
      . fmap (\p -> (p, topo M.!? p))
      . concatMap cardinalNeighbs
      $ curr

day10b :: Solution (Map (V2 Int) Int) Int
day10b = Solution{sParse = Right . parse, sShow = show, sSolve = Right . solveB}
