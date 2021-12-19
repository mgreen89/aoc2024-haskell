{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module AoC.Challenge.Day18
  ( day18a
  -- , day18b
  ) where

import           AoC.Solution
import           Control.Applicative            ( (<|>) )
import           Control.DeepSeq                ( NFData )
import           Data.Bifunctor                 ( first )
import           Data.List                      ( foldl1' )
import           GHC.Generics                   ( Generic )
import           Numeric                        ( readDec )
import           Text.Printf                    ( printf )

{- Create a binary tree of snail numbers, and user zippers to walk
   around the tree.
-}

data SnailNum = Pair SnailNum SnailNum
              | Num Int
  deriving (Show, Generic, NFData)

parseSnailNum :: String -> SnailNum
parseSnailNum = fst . go
 where
  go :: String -> (SnailNum, String)
  go []        = error "Shouldn't call with empty string"
  go s@(h : r) = if h == '['
    then
      let (left , rest ) = go r
          -- Drop the comma in the middle.
          (right, rest') = go (tail rest)
          -- Drop the ']' in the rest.
      in  (Pair left right, tail rest')
    else first Num . head . readDec $ s

showSnailNum :: SnailNum -> String
showSnailNum (Num x   ) = printf "%d" x
showSnailNum (Pair l r) = printf "[%s,%s]" (showSnailNum l) (showSnailNum r)

data Crumb = LeftCrumb SnailNum
           | RightCrumb SnailNum
  deriving (Show)

type Zipper = (SnailNum, [Crumb])

zipper :: SnailNum -> Zipper
zipper s = (s, [])

unzipper :: Zipper -> SnailNum
unzipper = fst . top

up :: Zipper -> Maybe Zipper
up (_, []               ) = Nothing
up (s, RightCrumb l : cs) = Just (Pair l s, cs)
up (s, LeftCrumb r : cs ) = Just (Pair s r, cs)

top :: Zipper -> Zipper
top z@(_, []) = z
top z         = maybe z top (up z)

downLeft :: Zipper -> Maybe Zipper
downLeft (Pair l r, cs) = Just (l, LeftCrumb r : cs)
downLeft (Num _   , _ ) = Nothing

downRight :: Zipper -> Maybe Zipper
downRight (Pair l r, cs) = Just (r, RightCrumb l : cs)
downRight (Num _   , _ ) = Nothing

bottomRight :: Zipper -> Maybe Zipper
bottomRight z@(Num _   , _) = Just z
bottomRight z@(Pair _ _, _) = bottomRight =<< downRight z

bottomLeft :: Zipper -> Maybe Zipper
bottomLeft z@(Num _   , _) = Just z
bottomLeft z@(Pair _ _, _) = bottomLeft =<< downLeft z

nextLeft :: Zipper -> Maybe Zipper
nextLeft z@(_, LeftCrumb _ : _ ) = nextLeft =<< up z
nextLeft z@(_, RightCrumb _ : _) = bottomRight =<< downLeft =<< up z
nextLeft (  _, []              ) = Nothing

nextRight :: Zipper -> Maybe Zipper
nextRight z@(_, LeftCrumb _ : _ ) = bottomLeft =<< downRight =<< up z
nextRight z@(_, RightCrumb _ : _) = nextRight =<< up z
nextRight (  _, []              ) = Nothing

modify :: (Int -> Int) -> Zipper -> Zipper
modify f (Num i, cs) = (Num (f i), cs)
modify _ _           = error "Tried to modify a non literal"

replace :: SnailNum -> Zipper -> Zipper
replace s' (_, cs) = (s', cs)

-- Walk the snail number, checking if any pairs are nested inside
-- four other pairs. If found, go to the most-left nested number and
-- return the pair it's in.
findExplode :: SnailNum -> Maybe Zipper
findExplode = go 0 . zipper
 where
  go :: Int -> Zipper -> Maybe Zipper
  go _ (  Num _   , _) = Nothing
  go 4 z@(Pair _ _, _) = up =<< bottomLeft z
  go i z@(Pair _ _, _) =
    (go (i + 1) =<< downLeft z) <|> (go (i + 1) =<< downRight z)

explode :: SnailNum -> Maybe SnailNum
explode s = do
  explodeLoc <- findExplode s
  let (Pair (Num lv) (Num rv), _) = explodeLoc
  repL <- case nextLeft explodeLoc of
    Just z  -> up =<< nextRight (modify (+ lv) z)
    Nothing -> Just explodeLoc
  repLR <- case nextRight repL of
    Just z  -> up =<< nextLeft (modify (+ rv) z)
    Nothing -> Just repL
  pure $ unzipper (replace (Num 0) repLR)

findSplit :: SnailNum -> Maybe Zipper
findSplit = go . zipper
 where
  go z@(Num x, _) | x >= 10   = Just z
                  | otherwise = Nothing
  go z@(Pair _ _, _) = (go =<< downLeft z) <|> (go =<< downRight z)

split :: SnailNum -> Maybe SnailNum
split s = do
  splitLoc <- findSplit s
  let (Num v, _) = splitLoc
      (d    , r) = v `divMod` 2
  pure $ unzipper (replace (Pair (Num d) (Num (d + r))) splitLoc)

add :: SnailNum -> SnailNum -> SnailNum
add s = reduce . Pair s

reduce :: SnailNum -> SnailNum
reduce s = maybe s reduce (explode s <|> split s)

sumSnail :: [SnailNum] -> SnailNum
sumSnail = foldl1' add

magnitude :: SnailNum -> Int
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r
magnitude (Num x   ) = x

day18a :: Solution _ _
day18a = Solution { sParse = Right . fmap parseSnailNum . lines
                  , sShow  = show
                  , sSolve = Right . magnitude . sumSnail
                  }

day18b :: Solution _ _
day18b = Solution { sParse = Right, sShow = show, sSolve = Right }
