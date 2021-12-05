module AoC.Challenge.Day04
  ( day04a
  , day04b
  ) where

import           AoC.Solution
import           AoC.Util
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.List.Split                ( splitOn )
import           Data.Maybe
import           Data.Traversable
import           Text.Read


wins :: [IntSet]
wins = fmap
  IS.fromList
  [ [0, 1, 2, 3, 4]
  , [5, 6, 7, 8, 9]
  , [10, 11, 12, 13, 14]
  , [15, 16, 17, 18, 19]
  , [20, 21, 22, 23, 24]
  , [0, 5, 10, 15, 20]
  , [1, 6, 11, 16, 21]
  , [2, 7, 12, 17, 22]
  , [3, 8, 13, 18, 23]
  , [4, 9, 14, 19, 24]
  ]

-- Map of value to position on the board.
type Board = IntMap Int

type Game = ([Int], [Board])

parseGame :: String -> Either String ([Int], [Board])
parseGame inp = do
  let drawinp : boardinp = splitOn "\n\n" inp
  draws  <- traverse readEither $ splitOn "," drawinp
  boards <-
    for boardinp
    $ fmap (IM.fromList . flip zip [0 ..])
    . traverse readEither
    . words
  pure (draws, boards)

stepBoard :: Int -> (Board, IntSet, Maybe Int) -> (Board, IntSet, Maybe Int)
stepBoard inp (b, marked, _) =
  let pos      = IM.lookup inp b
      marked'  = maybe id IS.insert pos marked
      finished = any (`IS.isSubsetOf` marked') wins
      tot      = sum . IM.keys . IM.filter (`IS.notMember` marked') $ b
  in  (b, marked', if finished then Just (tot * inp) else Nothing)

getFirst :: Game -> Maybe Int
getFirst (inps, boards) = do
  let states = fmap (, IS.empty, Nothing) boards
  sols <- go inps states
  (\(_, _, t) -> t) . head . snd $ sols
 where
  go
    :: [Int]
    -> [(Board, IntSet, Maybe Int)]
    -> Maybe ([Int], [(Board, IntSet, Maybe Int)])
  go is bs = case filter (\(_, _, c) -> isJust c) bs of
    [] -> case is of
      []    -> Nothing
      a : b -> go b (fmap (stepBoard a) bs)
    sols -> Just ([], sols)

day04a :: Solution Game Int
day04a = Solution { sParse = parseGame
                  , sShow  = show
                  , sSolve = maybeToEither "No games ended" . getFirst
                  }

getLast :: Game -> Maybe Int
getLast (inps, boards) = do
  let states = fmap (, IS.empty, Nothing) boards
  sols <- go inps states
  tot  <- (\(a, b) -> finish a (head b)) sols
  (\(_, _, t) -> t) tot
 where
  go
    :: [Int]
    -> [(Board, IntSet, Maybe Int)]
    -> Maybe ([Int], [(Board, IntSet, Maybe Int)])
  go is bs =
    let stillGoing = filter (\(_, _, t) -> isNothing t) bs
    in  if length stillGoing == 1
          then Just (is, stillGoing)
          else case is of
            []    -> Nothing
            a : b -> go b (fmap (stepBoard a) stillGoing)

  finish
    :: [Int] -> (Board, IntSet, Maybe Int) -> Maybe (Board, IntSet, Maybe Int)
  finish is d@(b, m, t)
    | isJust t = Just d
    | otherwise = case is of
      []    -> Nothing
      a : b -> finish b (stepBoard a d)

day04b :: Solution Game Int
day04b = Solution { sParse = parseGame
                  , sShow  = show
                  , sSolve = maybeToEither "No games ended" . getLast
                  }
