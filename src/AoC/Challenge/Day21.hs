{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module AoC.Challenge.Day21
  ( day21a
  -- , day21b
  ) where

import           AoC.Solution
import           AoC.Util                       ( listTo2Tuple )
import           Control.Monad                  ( (<=<) )
import           Data.Bifunctor                 ( second )
import           Text.Read                      ( readEither )

parse :: String -> Either String (Int, Int)
parse = listTo2Tuple <=< traverse (readEither . pure . last) . lines

newtype Die = Die { ddState :: [Int] }

instance Show Die where
  show (Die d) = "Die (" <> show (head d) <> ")"

detDie :: Die
detDie = Die $ cycle [1 .. 100]

-- Roll N times.
rollN :: Int -> Die -> ([Int], Die)
rollN n (Die rs) = second Die . splitAt n $ rs

play :: Die -> (Int, Int) -> Int
play d (p1Pos, p2Pos) = play' ((p1Pos, 0), (p2Pos, 0), 0, d)
 where
  play' :: ((Int, Int), (Int, Int), Int, Die) -> Int
  play' s =
    let s'@((_, p1Score), (_, p2Score), tot, _) = go s
    in  if p1Score >= 1000
          then p2Score * tot
          else if p2Score >= 1000 then p1Score * tot else play' s'

  go
    :: ((Int, Int), (Int, Int), Int, Die) -> ((Int, Int), (Int, Int), Int, Die)
  go (p1@(_, p1Score), p2@(_, p2Score), totRolls, d) =
    let (    p1Rolls, d'      ) = rollN 3 d
        p1'@(_      , p1Score') = move p1 (sum p1Rolls)
    in  if p1Score' >= 1000
          then (p1', p2, totRolls + 3, d')
          else
            let (    p2Rolls, d''     ) = rollN 3 d'
                p2'@(_      , p2Score') = move p2 (sum p2Rolls)
            in  (p1', p2', totRolls + 6, d'')

  move :: (Int, Int) -> Int -> (Int, Int)
  move (pos, score) r =
    let newPos = ((pos - 1 + r) `mod` 10) + 1 in (newPos, score + newPos)

day21a :: Solution (Int, Int) Int
day21a =
  Solution { sParse = parse, sShow = show, sSolve = Right . play detDie }

day21b :: Solution _ _
day21b = Solution { sParse = Right, sShow = show, sSolve = Right }
