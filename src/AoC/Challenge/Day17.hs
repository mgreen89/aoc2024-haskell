module AoC.Challenge.Day17
  ( day17a
  , day17b
  ) where

import           AoC.Solution
import           AoC.Util                       ( Point )
import           Data.Bifunctor                 ( first )
import           Data.Void                      ( Void )
import           Linear.V2                      ( V2(..) )
import           Linear.Vector                  ( zero )
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MP
import qualified Text.Megaparsec.Char.Lexer    as MPL

parser :: MP.Parsec Void String (V2 Point)
parser = do
  MP.string "target area: x="
  xMin <- signedInt
  MP.string ".."
  xMax <- signedInt
  MP.string ", y="
  yMin <- signedInt
  MP.string ".."
  yMax <- signedInt
  pure $ V2 (V2 xMin yMin) (V2 xMax yMax)
  where signedInt = MPL.signed MP.space MPL.decimal

parse :: String -> Either String (V2 Point)
parse = first MP.errorBundlePretty . MP.parse parser "day17"

{- This is pretty trivial without writing a program, but here
   for completeness.

   First, the x and y can be considered completely independently, and
   for part A we can just ignore the x component completely.

   The y component always forms a parabola w.r.t time (as the derivative
   is a constant -1).  This means it always gets back to y=0 at some point,
   having risen to a max height that's a triangular number.

   To get the answer, find the y bound that's furthest away from y=0. That
   distance must be the largest single step to maximize the height. If it's
   above zero, just need the nth triangular number where n is that step. If
   it's below zero, need the (n-1)th as (n-1) will be the final step
   above y=0.
-}
partA :: V2 Point -> Int
partA (V2 (V2 _ yMin) (V2 _ yMax)) =
  let y = if abs yMin > abs yMax then yMin else yMax
      tri x = (x * (x + 1)) `div` 2
  in  if y > 0 then tri y else tri (abs y - 1)

day17a :: Solution (V2 Point) Int
day17a = Solution { sParse = parse, sShow = show, sSolve = Right . partA }

-- Step the position and velocity.
step :: (Point, Point) -> (Point, Point)
step (pos, vel@(V2 vx vy)) = (pos + vel, V2 (vx - signum vx) (vy - 1))

-- Fire the probe!
-- Return a list of all the points it passes through until
-- it gets past the target.
fire :: Int -> Int -> Point -> [Point]
fire xMax yMin = fmap fst . takeWhile ok . iterate step . (zero, )
  where ok (V2 px py, _) = px <= xMax && py >= yMin

partB :: V2 Point -> Int
partB target@(V2 (V2 xMin yMin) (V2 xMax yMax)) =
  length
    . filter id
    . fmap (any inTarget)
    $ [ fire xMax yMin (V2 vx vy)
      | vx <- [xMinVel .. xMaxVel]
      , vy <- [yMinVel .. yMaxVel]
      ]
 where
    -- Assumes xMin is positive!
    -- If xMin is negative this will just bork (sqrt a negative).
  xMinVel :: Int
  xMinVel = (floor :: Double -> Int) . sqrt . (* 2) . fromIntegral $ xMin
  xMaxVel :: Int
  xMaxVel = xMax
  -- If yMin is positive, this could be tighter.
  yMinVel :: Int
  yMinVel = yMin
  yMaxVel :: Int
  yMaxVel = partA target

  inTarget :: Point -> Bool
  inTarget (V2 x y) = x >= xMin && x <= xMax && y >= yMin && y <= yMax

day17b :: Solution (V2 Point) Int
day17b = Solution { sParse = parse, sShow = show, sSolve = Right . partB }
