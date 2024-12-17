module AoC.Challenge.Day16 (
  day16a,
  day16b,
)
where

import AoC.Common.Graph (aStar, explore)
import AoC.Common.Point (Dir (..), dirPoint, manhattan, parse2dCharMap)
import AoC.Solution
import AoC.Util (maybeToEither)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
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

neighbs :: Set (V2 Int) -> (V2 Int, Dir) -> Map (V2 Int, Dir) Int
neighbs walls (p, d) =
  let straight =
        ( if (p + dirPoint d) `S.member` walls
            then M.empty
            else M.singleton (p + dirPoint d, d) 1
        )
   in M.union
        straight
        ( M.fromList
            [ ((p, L <> d), 1000)
            , ((p, R <> d), 1000)
            ]
        )

solveA :: (Set (V2 Int), V2 Int, V2 Int) -> Maybe Int
solveA (walls, s, e) =
  aStar heur (neighbs walls) (s, R) atEnd
 where
  heur = manhattan e . fst
  atEnd = (== e) . fst

day16a :: Solution (Set (V2 Int), V2 Int, V2 Int) Int
day16a = Solution{sParse = Right . parse, sShow = show, sSolve = maybeToEither "failed" . solveA}

solveB :: (Set (V2 Int), V2 Int, V2 Int) -> Int
solveB (walls, s@(V2 sx sy), e@(V2 ex ey)) =
  let
    minLength = fromJust $ solveA (walls, s, e)
    fromS = explore (neighbs walls) (s, R)
    fromE = explore (neighbs walls) (e, D)
   in
    S.size . S.fromList $
      [ V2 x y
      | x <- [sx .. ex]
      , y <- [ey .. sy]
      , d <- [U ..]
      , ((+) <$> fromS M.!? (V2 x y, d) <*> fromE M.!? (V2 x y, d <> D)) == Just minLength
      ]

day16b :: Solution (Set (V2 Int), V2 Int, V2 Int) Int
day16b = Solution{sParse = Right . parse, sShow = show, sSolve = Right . solveB}
