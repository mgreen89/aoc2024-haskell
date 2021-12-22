module AoC.Challenge.Day21
  ( day21a
  , day21b
  ) where

import           AoC.Solution
import           AoC.Util                       ( freqs
                                                , listTo2Tuple
                                                )
import           Control.Monad                  ( (<=<)
                                                , replicateM
                                                )
import           Data.Bifunctor                 ( first
                                                , second
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Text.Read                      ( readEither )

parse :: String -> Either String (Int, Int)
parse = listTo2Tuple <=< traverse (readEither . pure . last) . lines

-- | Loop until a Left value is hit.
loopUntilLeft :: (a -> Either b a) -> a -> b
loopUntilLeft f x = case f x of
  Left  v  -> v
  Right x' -> loopUntilLeft f x'

newtype Die = Die { ddState :: [Int] }

instance Show Die where
  show (Die d) = "Die (" <> show (head d) <> ")"

detDie :: Die
detDie = Die { ddState = cycle [1 .. 100] }

-- Roll N times.
rollN :: Int -> Die -> ([Int], Die)
rollN n (Die rs) = second Die . splitAt n $ rs

type Player = (Int, Int)
type Game = ((Int, Int), (Int, Int), Int)

initGame :: (Int, Int) -> Game
initGame (p1Pos, p2Pos) = ((p1Pos, 0), (p2Pos, 0), 0)

move :: Player -> Int -> Player
move (pos, score) r =
  let pos' = ((pos - 1 + r) `mod` 10) + 1 in (pos', score + pos')

turn :: Game -> Int -> Game
turn (p1, p2, turns) r | even turns = (move p1 r, p2, turns + 1)
                       | otherwise  = (p1, move p2 r, turns + 1)

play :: Die -> (Int, Int) -> Int
play die inp = loopUntilLeft go (initGame inp, die)
 where
  go :: (Game, Die) -> Either Int (Game, Die)
  go gs@(((_, p1Score), (_, p2Score), turns), _)
    | p1Score >= 1000 = Left $ p2Score * turns * 3
    | p2Score >= 1000 = Left $ p1Score * turns * 3
    | otherwise       = Right $ goTurn gs

  goTurn :: (Game, Die) -> (Game, Die)
  goTurn (g, d) = let (rolls, d') = rollN 3 d in (turn g (sum rolls), d')

day21a :: Solution (Int, Int) Int
day21a =
  Solution { sParse = parse, sShow = show, sSolve = Right . play detDie }

{- Possible die rolls x3:
  Roll   |  3   4   5   6   7   8   9
  # ways |  1   3   6   7   6   3   1
-}
diracRolls :: [(Int, Int)]
diracRolls = M.toList . freqs . fmap sum . replicateM 3 $ [1 .. 3]

-- Create a memoized map of game status to number of wins for each player.
playDirac :: Map Game (Int, Int) -> Game -> (Map Game (Int, Int), (Int, Int))
playDirac m game@((_, p1Score), (_, p2Score), _)
  | p1Score >= 21 = (m, (1, 0))
  | p2Score >= 21 = (m, (0, 1))
  | otherwise = case M.lookup game m of
    Just w  -> (m, w)
    Nothing -> (M.insert game wins m', wins)
 where
  (m', wins) = foldl go (m, (0, 0)) (first (turn game) <$> diracRolls)
  go (mp, (w1, w2)) (g, c) =
    let (mp', (w1', w2')) = playDirac mp g
    in  (mp', (w1 + c * w1', w2 + c * w2'))

day21b :: Solution (Int, Int) Int
day21b = Solution
  { sParse = parse
  , sShow  = show
  , sSolve = Right . uncurry max . snd . playDirac M.empty . initGame
  }
