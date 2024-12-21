module AoC.Challenge.Day21 (
  day21a,
  day21b,
)
where

import AoC.Common (pairs)
import AoC.Common.Point (parse2dCharMap)
import AoC.Solution
import Data.Map (Map)
import qualified Data.Map as M
import Data.MemoTrie as Memo
import Linear (V2 (..))

numericStr :: String
numericStr =
  "789\n\
  \456\n\
  \123\n\
  \ 0A"

directionStr :: String
directionStr =
  " ^A\n\
  \<v>"

numPad :: Map (V2 Int) Char
numPad = M.filter (/= ' ') $ parse2dCharMap numericStr

dirPad :: Map (V2 Int) Char
dirPad = M.filter (/= ' ') $ parse2dCharMap directionStr

-- Best path should be left first, then down, then up, then right, if possible.
-- This doesn't help the current level but makes the next level faster.
bestPath :: Map (V2 Int) Char -> Char -> Char -> String
bestPath pad fromC toC = go initFrom initTo []
 where
  go :: V2 Int -> V2 Int -> String -> String
  go from to a
    | d == V2 0 0 = a ++ "A"
    | dx < 0 && M.member moveX pad = go moveX to (a ++ replicate (-dx) '<')
    | dy > 0 && M.member moveY pad = go moveY to (a ++ replicate dy 'v')
    | dy < 0 && M.member moveY pad = go moveY to (a ++ replicate (-dy) '^')
    | dx > 0 && M.member moveX pad = go moveX to (a ++ replicate dx '>')
    | otherwise = error "Invalid move"
   where
    d@(V2 dx dy) = to - from
    moveX = from + V2 dx 0
    moveY = from + V2 0 dy
  rev = M.fromList [(c, p) | (p, c) <- M.toList pad]
  initFrom = rev M.! fromC
  initTo = rev M.! toC

numMoves :: Map (Char, Char) String
numMoves =
  M.fromList
    [ ((s, e), bestPath numPad s e)
    | s <- "A0123456789"
    , e <- "A0123456789"
    ]

dirMoves :: Map (Char, Char) String
dirMoves =
  M.fromList
    [ ((s, e), bestPath dirPad s e)
    | s <- "<v^>A"
    , e <- "<v^>A"
    ]

solve :: Int -> [String] -> Int
solve nDirRobots = sum . fmap (\c -> complexity c (getBest c))
 where
  getBest s =
    let
      numToDir = concatMap (numMoves M.!) (pairs ('A' : s))
     in
      go nDirRobots numToDir

  complexity c pathlen = read (init c) * pathlen

  go :: Int -> String -> Int
  go = Memo.memo2 go2

  go2 :: Int -> String -> Int
  go2 d s
    | d == 0 = length s
    | otherwise =
        sum
          [ go (d - 1) s'
          | s' <- (dirMoves M.!) <$> pairs ('A' : s)
          ]

day21a :: Solution [String] Int
day21a = Solution{sParse = Right . lines, sShow = show, sSolve = Right . solve 2}

day21b :: Solution [String] Int
day21b = Solution{sParse = Right . lines, sShow = show, sSolve = Right . solve 25}
