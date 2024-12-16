module AoC.Challenge.Day15 (
  day15a,
  day15b,
)
where

import AoC.Common (listTup2)
import AoC.Common.Point (Dir (..), dirFromArrow, dirPoint, parse2dCharMap)
import AoC.Solution
import AoC.Util (maybeToEither)
import Data.Foldable (foldMap', foldl')
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
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

gps' :: Map (V2 Int) (V2 Int) -> V2 Int -> Int
gps' bs b@(V2 x _) =
  case bs M.!? b of
    Just (V2 x' _) | x' == x + 1 -> gps b
    _ -> 0

stepB :: Set (V2 Int) -> (Map (V2 Int) (V2 Int), V2 Int) -> Dir -> (Map (V2 Int) (V2 Int), V2 Int)
stepB walls (boxes, robot) dir =
  fromMaybe
    (boxes, robot)
    ( case dir of
        U -> goV [] [robot'] 0
        -- R -> goR [] [robot'] 0
        -- D -> goV [] [robot'] 0
        -- L -> goL [] [robot'] 0)
    )
 where
  robot' = robot + dirPoint dir
  goV :: [V2 Int] -> [V2 Int] -> Int -> Maybe (Map (V2 Int) (V2 Int), V2 Int)
  goV old new ofst = Nothing

{-
 for b in bs:
   if b is empty, tick.
   if b is wall, fail.
   if b is box, add both box things.
-}
{-let old' = old ++ new
    p = robot' + (ofst *^ dirPoint dir)
 in if any (`S.member` walls) new
      then Nothing
      else concatMap (\p@(V2 x y) -> if M.member p boxes then [] else [])
        if S.member p boxes
          then go (nbs + 1)
          else
            if nbs == 0
              then (boxes, robot')
              else (S.insert p . S.delete robot' $ boxes, robot')-}

solveB :: ((Set (V2 Int), Set (V2 Int), V2 Int), [Dir]) -> Int
solveB ((walls, boxes, robot), insts) =
  let
    walls' =
      foldMap'
        ( \(V2 x y) ->
            S.fromList [V2 (2 * x) y, V2 (2 * x + 1) y]
        )
        walls
    boxes' =
      foldMap'
        ( \(V2 x y) ->
            M.fromList
              [ (V2 (2 * x) y, V2 (2 * x + 1) y)
              , (V2 (2 * x + 1) y, V2 (2 * x) y)
              ]
        )
        boxes
    robot' = (\(V2 x y) -> V2 (2 * x) y) robot
   in
    sum
      . (\bs -> fmap (gps' bs) (M.keys bs))
      . fst
      . foldl' (stepB walls') (boxes', robot')
      $ insts

day15b :: Solution ((Set (V2 Int), Set (V2 Int), V2 Int), [Dir]) Int
day15b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB}
