module AoC.Challenge.Day23
  ( day23a
  , day23b
  ) where

import           AoC.Solution
import           AoC.Util                       ( aStar
                                                , maybeToEither
                                                )
import           Control.DeepSeq                ( NFData )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isNothing
                                                , maybeToList
                                                )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           GHC.Generics                   ( Generic )

type Hallway = Vector (Maybe Char)
type Room = [Char]
type Rooms = Vector Room

data Burrow = Burrow
  { hallway :: Vector (Maybe Char)
  , rooms   :: Vector [Char]
  }
  deriving (Show, Eq, Ord, Generic, NFData)

burrowA :: Burrow
burrowA = Burrow { hallway = V.replicate 7 Nothing
                 , rooms   = V.fromList ["DC", "CD", "AA", "BB"]
                 }

burrowB :: Burrow
burrowB = Burrow { hallway = V.replicate 7 Nothing
                 , rooms   = V.fromList ["DDDC", "CCBD", "ABAA", "BACB"]
                 }

room :: Char -> Int
room 'A' = 0
room 'B' = 1
room 'C' = 2
room 'D' = 3
room x   = error $ "Unknown room: " <> pure x

moveCost :: Char -> Int
moveCost 'A' = 1
moveCost 'B' = 10
moveCost 'C' = 100
moveCost 'D' = 1000
moveCost x   = error $ "Unknown amphipod: " <> pure x

hallwayMoves :: [[Int]]
hallwayMoves =
  [ [2, 1, 1, 3, 5, 7, 8]
  , [4, 3, 1, 1, 3, 5, 6]
  , [6, 5, 3, 1, 1, 3, 4]
  , [8, 7, 5, 3, 1, 1, 2]
  ]

hallwayReachable :: Hallway -> Int -> Int -> Bool
hallwayReachable hallway room hallPos
  | room < 0 || hallPos < 0 = error "Invalid room or hall position"
  | room > 3 || hallPos > 6 = error "Invalid room or hall position"
  | room + 1 >= hallPos = all (isNothing . (hallway V.!)) [hallPos .. room + 1]
  | otherwise = all (isNothing . (hallway V.!)) [room + 2 .. hallPos]

roomAvailable :: Rooms -> Char -> Bool
roomAvailable rooms c = all (== c) (rooms V.! room c)

roomReachable :: Hallway -> Int -> Int -> Bool
roomReachable hallway hallPos room
  | room < 0 || hallPos < 0 = error "Invalid room or hall position"
  | room > 3 || hallPos > 6 = error "Invalid room or hall position"
  | room + 1 >= hallPos + 1 = all (isNothing . (hallway V.!))
                                  [hallPos + 1 .. room + 1]
  | hallPos - 1 >= room + 2 = all (isNothing . (hallway V.!))
                                  [room + 2 .. hallPos - 1]
  | otherwise = True

neighbours :: Int -> Burrow -> [(Burrow, Int)]
neighbours roomSize Burrow {..} = intoHallway ++ outOfHallway
 where
  intoHallway =
    [ (Burrow h' rs', cost)
    | roomI <- [0 .. 3]
    , let roomChar = "ABCD" !! roomI
    , let r        = rooms V.! roomI
    , not (null r)
    , r /= replicate (length r) roomChar
    , let (x : xs) = r
    , hallPos <- [0 .. 6]
    , hallwayReachable hallway roomI hallPos
    , let h'    = hallway V.// [(hallPos, Just x)]
          rs'   = rooms V.// [(roomI, xs)]
          moves = (roomSize - length xs) + (hallwayMoves !! roomI !! hallPos)
          cost  = moves * moveCost x
    ]

  outOfHallway =
    [ (Burrow h' rs', cost)
    | hallPos <- [0 .. 6]
    , let h' = hallway V.// [(hallPos, Nothing)]
    , x <- maybeToList $ hallway V.! hallPos
    , let roomI = room x
          r     = rooms V.! roomI
    , roomAvailable rooms x
    , roomReachable hallway hallPos roomI
    , let r'    = x : r
          rs'   = rooms V.// [(roomI, r')]
          moves = (hallwayMoves !! roomI !! hallPos) + (roomSize - length r)
          cost  = moves * moveCost x
    ]

heuristic :: Burrow -> Int
heuristic Burrow {..} = sum
  [ moveCost x * abs (roomI - room x) * 2
  | roomI <- [0 .. 3]
  , let r = rooms V.! roomI
  , x <- r
  ]

targetBurrow :: Int -> Burrow
targetBurrow roomSize = Burrow
  { hallway = V.replicate 7 Nothing
  , rooms   = V.fromList $ replicate roomSize <$> "ABCD"
  }

run :: Burrow -> Int -> Maybe Int
run b roomSize =
  aStar heuristic (M.fromList . neighbours roomSize) b (targetBurrow roomSize)

day23a :: Solution (Burrow, Int) Int
day23a = Solution { sParse = const $ Right (burrowA, 2)
                  , sShow  = show
                  , sSolve = maybeToEither "aStar failed" . uncurry run
                  }

day23b :: Solution (Burrow, Int) Int
day23b = Solution { sParse = const $ Right (burrowB, 4)
                  , sShow  = show
                  , sSolve = maybeToEither "aStar failed" . uncurry run
                  }
