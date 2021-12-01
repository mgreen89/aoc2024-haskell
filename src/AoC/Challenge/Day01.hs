{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module AoC.Challenge.Day01
  ( day01a
  , day01b
  ) where

import           AoC.Solution


day01acount :: Integer -> [Integer] -> Integer
day01acount s (a : rest@(b : c)) =
  if b > a then day01acount (s + 1) rest else day01acount s rest
day01acount s _ = s


day01bcount :: Integer -> [Integer] -> Integer
day01bcount s l@(a : b : c : d : e) = if (b + c + d) > (a + b + c)
  then day01bcount (s + 1) (tail l)
  else day01bcount s (tail l)
day01bcount s _ = s


day01a :: Solution [Integer] Integer
day01a = Solution { sParse = Right . fmap read . lines
                  , sShow  = show
                  , sSolve = Right . day01acount 0
                  }

day01b :: Solution _ _
day01b = Solution { sParse = Right . fmap read . lines
                  , sShow  = show
                  , sSolve = Right . day01bcount 0
                  }
