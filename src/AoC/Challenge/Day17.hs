{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module AoC.Challenge.Day17
  ( day17a
  -- , day17b
  ) where

import           AoC.Solution
import           Data.Bifunctor                 ( first )
import           Data.Void                      ( Void )
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MP
import qualified Text.Megaparsec.Char.Lexer    as MPL

parser :: MP.Parsec Void String ((Int, Int), (Int, Int))
parser = do
  MP.string "target area: x="
  xMin <- signedInt
  MP.string ".."
  xMax <- signedInt
  MP.string ", y="
  yMin <- signedInt
  MP.string ".."
  yMax <- signedInt
  pure ((xMin, xMax), (yMin, yMax))
  where signedInt = MPL.signed MP.space MPL.decimal

parse :: String -> Either String ((Int, Int), (Int, Int))
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
partA :: (Int, Int) -> Int
partA (yMin, yMax) =
  let y = if abs yMin > abs yMax then yMin else yMax
      tri x = (x * (x + 1)) `div` 2
  in  if y > 0 then tri y else tri (abs y - 1)

day17a :: Solution ((Int, Int), (Int, Int)) Int
day17a =
  Solution { sParse = parse, sShow = show, sSolve = Right . partA . snd }

day17b :: Solution _ _
day17b = Solution { sParse = parse, sShow = show, sSolve = Right }
