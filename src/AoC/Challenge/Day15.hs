{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day15 (
  day15a,
)
where

-- day15a
-- , day15b

import AoC.Common (listTup2)
import AoC.Common.Point (Dir (..), dirFromArrow, dirPoint, parse2dCharMap)
import AoC.Solution
import AoC.Util (maybeToEither)
import Data.Foldable (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..), (*^))

parse :: String -> Either String ((Set (V2 Int), Set (V2 Int), V2 Int), [Dir])
parse inp = do
  (rawMap, rawInst) <- maybeToEither "Invalid input" . listTup2 . splitOn "\n\n" $ inp
  inst <- traverse dirFromArrow (concat $ lines rawInst)
  let mp = M.filter (/= '.') $ parse2dCharMap rawMap
  let (rs, rest) = M.partition (== '@') mp
  r <- maybeToEither "Missing robot!" (fmap fst . M.lookupMin $ rs)
  let (walls, boxes) = M.partition (== '#') rest
  pure ((M.keysSet walls, M.keysSet boxes, r), inst)

step :: Set (V2 Int) -> (Set (V2 Int), V2 Int) -> Dir -> (Set (V2 Int), V2 Int)
step walls (boxes, robot) dir =
  go 0
 where
  robot' = robot + dirPoint dir
  go nbs =
    let p = robot' + (nbs *^ dirPoint dir)
     in if S.member p walls
          then (boxes, robot)
          else
            if S.member p boxes
              then go (nbs + 1)
              else
                if nbs == 0
                  then (boxes, robot')
                  else (S.insert p . S.delete robot' $ boxes, robot')

gps :: V2 Int -> Int
gps (V2 x y) = x + y * 100

solveA :: ((Set (V2 Int), Set (V2 Int), V2 Int), [Dir]) -> Int
solveA ((walls, boxes, robot), insts) =
  sum . fmap gps . S.toList . fst . foldl' (step walls) (boxes, robot) $ insts

day15a :: Solution ((Set (V2 Int), Set (V2 Int), V2 Int), [Dir]) Int
day15a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}

day15b :: Solution _ _
day15b = Solution{sParse = Right, sShow = show, sSolve = Right}
