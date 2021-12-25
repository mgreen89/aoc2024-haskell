module AoC.Challenge.Day25
  ( day25a
  ) where

import           AoC.Solution
import           AoC.Util                       ( Point )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Linear.V2                      ( V2(..) )

parse :: String -> Map Point Char
parse =
  M.fromList
    . concat
    . zipWith (\y -> zipWith (\x -> (V2 x y :: Point, )) [0 ..]) [0 ..]
    . lines

incPos :: Char -> Point -> Point -> Point
incPos c (V2 xMax yMax) (V2 x y)
  | c == '>'  = V2 ((x + 1) `mod` xMax) y
  | c == 'v'  = V2 x ((y + 1) `mod` yMax)
  | otherwise = error $ "Invalid char: " <> pure c

movePossible :: Char -> Char -> Point -> Point -> Map Point Char -> Bool
movePossible d c bs p m
  | c == d    = let canMove = (m M.! incPos c bs p == '.') in canMove
  | otherwise = False

runSubStep :: Char -> Point -> Map Point Char -> Map Point Char
runSubStep c bounds m =
  let moveable = M.foldlWithKey
        (\a p v -> if movePossible c v bounds p m then p : a else a)
        []
        m
      oldPlaces = M.fromList $ fmap (, '.') moveable
      newPlaces = M.fromList $ fmap ((, c) . incPos c bounds) moveable
  in  M.unions [newPlaces, oldPlaces, m]

runStep :: Map Point Char -> Map Point Char
runStep m =
  let bounds = (+ V2 1 1) . fst . head . M.toDescList $ m
  in  runSubStep 'v' bounds . runSubStep '>' bounds $ m

run :: Map Point Char -> Int
run = fix 1
 where
  fix :: Int -> Map Point Char -> Int
  fix i m = let m' = runStep m in if m == m' then i else fix (i + 1) m'

day25a :: Solution (Map Point Char) Int
day25a =
  Solution { sParse = Right . parse, sShow = show, sSolve = Right . run }
