module AoC.Challenge.Day06 (
  day06a,
  day06b,
)
where

import AoC.Common.Point (
  Dir (..),
  boundingBox',
  dirPoint,
  dirRot,
  inBoundingBox,
  parse2dCharMap,
 )
import AoC.Solution
import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

parseMap :: (Char -> Maybe a) -> String -> Map (V2 Int) a
parseMap f =
  M.mapMaybe f . parse2dCharMap

parse :: String -> (Set (V2 Int), V2 Int)
parse = bimap M.keysSet (fst . M.findMin) . M.partition id . parseMap x
 where
  x :: Char -> Maybe Bool
  x = \case
    '#' -> Just True
    '^' -> Just False
    _ -> Nothing

step :: Set (V2 Int) -> (V2 Int, Dir) -> (V2 Int, Dir)
step blocks (p, d) =
  if (p + dirPoint d) `S.member` blocks
    then (p, dirRot d R)
    else (p + dirPoint d, d)

solveA :: (Set (V2 Int), V2 Int) -> Int
solveA (blocks, startPos) =
  S.size
    . S.fromList
    . takeWhile (inBoundingBox bb)
    . fmap fst
    . iterate (step blocks)
    $ (startPos, U)
 where
  bb = fromMaybe (V2 0 0, V2 0 0) (boundingBox' blocks)

day06a :: Solution (Set (V2 Int), V2 Int) Int
day06a = Solution{sParse = Right . parse, sShow = show, sSolve = Right . solveA}

stepWithHistory ::
  Set (V2 Int) ->
  ((V2 Int, Dir), Set (V2 Int, Dir)) ->
  Either ((V2 Int, Dir), Set (V2 Int, Dir)) ((V2 Int, Dir), Set (V2 Int, Dir))
stepWithHistory blocks ((p, d), h) =
  if (p', d') `S.member` h
    then Left ((p', d'), h)
    else Right ((p', d'), S.insert (p', d') h)
 where
  (p', d') = step blocks (p, d)

doesLoop :: (Set (V2 Int), V2 Int) -> Bool
doesLoop (blocks, startPos) =
  go (startPos, U) S.empty
 where
  go x h = case stepWithHistory blocks (x, h) of
    Left _ -> True
    Right ((p, d), h') -> inBoundingBox bb p && go (p, d) h'
  bb = fromMaybe (V2 0 0, V2 0 0) (boundingBox' blocks)

solveB :: (Set (V2 Int), V2 Int) -> Int
solveB (blocks, startPos) =
  length
    . filter id
    . fmap (doesLoop . (,startPos) . (`S.insert` blocks))
    $ possBlocks
 where
  bb = fromMaybe (V2 0 0, V2 0 0) (boundingBox' blocks)

  possBlocks =
    [ p
    | p <- initPath
    , not (S.member p blocks)
    , p /= startPos
    ]

  initPath = S.toList
    . S.fromList
    . takeWhile (inBoundingBox bb)
    . fmap fst
    . iterate (step blocks)
    $ (startPos, U)

day06b :: Solution (Set (V2 Int), V2 Int) Int
day06b = Solution{sParse = Right . parse, sShow = show, sSolve = Right . solveB}
