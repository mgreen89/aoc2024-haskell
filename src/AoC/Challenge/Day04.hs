module AoC.Challenge.Day04
  ( day04a
  , day04b
  ) where

import           AoC.Solution                   ( Solution(..) )
import           AoC.Util                       ( maybeToEither )
import           Data.Bifunctor                 ( first )
import           Data.Function                  ( on )
import           Data.Functor.Foldable          ( ana
                                                , cata
                                                )
import           Data.Functor.Foldable.TH       ( makeBaseFunctor )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( mapMaybe )
import           Data.Traversable               ( for )
import           Safe                           ( maximumByMay )
import           Text.Read                      ( readEither )


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

type G = ([Int], [Board])

parseGame :: String -> Either String G
parseGame inp = do
  let drawinp : boardinp = splitOn "\n\n" inp
  draws  <- traverse readEither $ splitOn "," drawinp
  boards <-
    for boardinp
    $ fmap (IM.fromList . flip zip [0 ..])
    . traverse readEither
    . words
  pure (draws, boards)

-- Useful reading for understanding what's going on here.
-- Part 1 and 2 of:
--  https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html

-- Definition for a game functor.
data Game = Turn Game
          | Finish Int
          | End

makeBaseFunctor ''Game

-- Algebra to turn the game functor into a Maybe (# turns taken, checksum).
scoreAlg :: GameF (Maybe (Int, Int)) -> Maybe (Int, Int)
scoreAlg = \case
  TurnF   x -> first (+ 1) <$> x
  FinishF x -> Just (1, x)
  EndF      -> Nothing

-- GameState to track the state of a single board.
data GameInfo = GameInfo
  { giDraws  :: [Int]
  , giMarked :: IntSet
  }

-- Coalgebra to generate a Game functor with each step of the game.
gameCoalg :: Board -> GameInfo -> GameF GameInfo
gameCoalg board GameInfo {..} = case giDraws of
  [] -> EndF
  a : rest ->
    let pos      = IM.lookup a board
        marked'  = maybe id IS.insert pos giMarked
        finished = any (`IS.isSubsetOf` marked') wins
        tot      = sum . IM.keys . IM.filter (`IS.notMember` marked') $ board
        chksum   = tot * a
    in  if finished then FinishF chksum else TurnF $ GameInfo rest marked'

initGame :: [Int] -> Board -> Game
initGame draws board = ana (gameCoalg board) (GameInfo draws IS.empty)

initGames :: G -> [Game]
initGames (draws, boards) = fmap (initGame draws) boards

finishGame :: Game -> Maybe (Int, Int)
finishGame = cata scoreAlg

-- Strict loop until a Left hit.
loopEither :: (a -> Either r a) -> a -> r
loopEither f = go
 where
  go !x = case f x of
    Left  r  -> r
    Right !y -> go y

-- Get the first game to finish.
-- Run a step on each game and check if any are done each step.
getFirst :: G -> Either String Int
getFirst = loopEither checkFinish . initGames
 where
  checkFinish :: [Game] -> Either (Either String Int) [Game]
  checkFinish = traverse $ \case
    Turn   i -> Right i
    Finish x -> Left (Right x)
    End      -> Left (Left "Game never finished")

day04a :: Solution G Int
day04a = Solution { sParse = parseGame, sShow = show, sSolve = getFirst }

-- Run all the games to completion, the find the one with the highest
-- required number of turns.
getLast :: G -> Either String Int
getLast =
  fmap snd
    . maybeToEither "No games finished"
    . maximumByMay (compare `on` fst)
    -- Could use 'hylo' here instead of (finish . init) (i.e. cata . ana),
    -- but given we already have initGames just do this to prevent code dupe.
    . mapMaybe finishGame
    . initGames

day04b :: Solution G Int
day04b = Solution { sParse = parseGame, sShow = show, sSolve = getLast }
