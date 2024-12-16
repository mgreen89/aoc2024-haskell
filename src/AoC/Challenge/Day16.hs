{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day16 (
  day16a,
)
where

-- day16a
-- , day16b

import AoC.Common.Graph (aStar)
import AoC.Common.Point (Dir (..), dirPoint, dirRot, manhattan, parse2dCharMap)
import AoC.Solution
import AoC.Util (maybeToEither)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

parse :: String -> (Set (V2 Int), V2 Int, V2 Int)
parse inp =
  let raw = parse2dCharMap inp
      (s, _) = M.findMin $ M.filter (== 'S') raw
      (e, _) = M.findMin $ M.filter (== 'E') raw
      walls = M.keysSet $ M.filter (== '#') raw
   in (walls, s, e)

solveA :: (Set (V2 Int), V2 Int, V2 Int) -> Maybe Int
solveA (walls, s, e) =
  aStar heur neighbs (s, R) atEnd
 where
  heur = manhattan e . fst
  atEnd = (== e) . fst
  neighbs (p, d) =
    let straight =
          ( if (p + dirPoint d) `S.member` walls
              then M.empty
              else M.singleton (p + dirPoint d, d) 1
          )
     in M.union
          straight
          ( M.fromList
              [ ((p, dirRot L d), 1000)
              , ((p, dirRot R d), 1000)
              ]
          )

day16a :: Solution (Set (V2 Int), V2 Int, V2 Int) Int
day16a = Solution{sParse = Right . parse, sShow = show, sSolve = maybeToEither "failed" . solveA}

day16b :: Solution _ _
day16b = Solution{sParse = Right, sShow = show, sSolve = Right}
