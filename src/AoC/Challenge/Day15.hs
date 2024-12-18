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

stepB :: Set (V2 Int) -> (Map (V2 Int) Bool, V2 Int) -> Dir -> (Map (V2 Int) Bool, V2 Int)
stepB walls (boxes, robot) dir =
  case maybeMove S.empty (S.singleton robot') of
    Just moved ->
      let toMove = M.restrictKeys boxes moved
          new = M.fromList . fmap (\(p, b) -> (p + dirPoint dir, b)) $ M.toList toMove
       in (M.union new (M.withoutKeys boxes moved), robot')
    Nothing -> (boxes, robot)
 where
  robot' :: V2 Int
  robot' = robot + dirPoint dir

  boxesKeys = M.keysSet boxes

  -- Returns the set of boxes to move if successful.
  maybeMove :: Set (V2 Int) -> Set (V2 Int) -> Maybe (Set (V2 Int))
  maybeMove old new
    | not (S.null (S.intersection walls new)) = Nothing
    | S.null (S.intersection new boxesKeys) = Just old
    | otherwise = maybeMove old' new'
   where
    expanded =
      S.fromList
        [ p
        | n <- S.toList (S.intersection boxesKeys new)
        , p <-
            if boxes M.!? n == Just True
              then [n, n + V2 1 0]
              else [n - V2 1 0, n]
        ]
    old' = old <> expanded
    new' = S.difference (S.map (+ dirPoint dir) expanded) expanded

solveB :: ((Set (V2 Int), Set (V2 Int), V2 Int), [Dir]) -> Int
solveB ((walls, boxes, robot), insts) =
  let
    walls' :: Set (V2 Int)
    walls' =
      foldMap'
        ( \(V2 x y) ->
            S.fromList [V2 (2 * x) y, V2 (2 * x + 1) y]
        )
        walls

    boxes' :: Map (V2 Int) Bool
    boxes' =
      foldMap'
        ( \(V2 x y) ->
            M.fromList
              [ (V2 (2 * x) y, True)
              , (V2 (2 * x + 1) y, False)
              ]
        )
        boxes

    robot' :: V2 Int
    robot' = (\(V2 x y) -> V2 (2 * x) y) robot
   in
    sum
      . fmap gps
      . M.keys
      . M.filter id
      . fst
      . foldl' (stepB walls') (boxes', robot')
      $ insts

day15b :: Solution ((Set (V2 Int), Set (V2 Int), V2 Int), [Dir]) Int
day15b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB}
