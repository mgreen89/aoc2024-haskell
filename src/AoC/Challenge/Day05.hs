module AoC.Challenge.Day05
  ( day05a
  , day05b
  ) where

import           AoC.Solution
import           Control.Monad.ST               ( ST
                                                , runST
                                                )
import           Data.Array.MArray             as A
import           Data.Array.ST                 as A
import           Data.Bifunctor                 ( first )
import           Data.Foldable                  ( for_
                                                , toList
                                                )
import           Data.Int                       ( Int8 )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Void                      ( Void )
import           Linear.V2                      ( V2(..) )
import           Linear.Vector                  ( (*^) )
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MP
import qualified Text.Megaparsec.Char.Lexer    as MPL

type Point = V2 Int
type Line = V2 Point

lineParser :: MP.Parsec Void String Line
lineParser = do
  x1 <- MPL.decimal
  MP.single ','
  y1 <- MPL.decimal
  MP.string " -> "
  x2 <- MPL.decimal
  MP.single ','
  y2 <- MPL.decimal
  pure $ V2 (V2 x1 y1) (V2 x2 y2)

parse :: String -> Either String [Line]
parse =
  first MP.errorBundlePretty . traverse (MP.parse lineParser "day05") . lines

-- | Get a list of all the points (with integer co-ords) on a given line
linePoints :: Line -> [Point]
linePoints (V2 p1 p2) = [ p1 + n *^ step | n <- [0 .. gcf] ]
 where
  d@(V2 dx dy) = p2 - p1
  gcf          = gcd dy dx
  step         = (`div` gcf) <$> d

-- | Check if a line is perpendicular (i.e. vertical or horizontal)
isPerp :: Line -> Bool
isPerp (V2 (V2 x1 y1) (V2 x2 y2)) = x1 == x2 || y1 == y2

-- | Create a map of elem -> frequency.
getFreqs :: (Foldable f, Ord a) => f a -> Map a Int
getFreqs = M.fromListWith (+) . map (, 1) . toList

-- Implementation using a Map
evalLines :: [Line] -> Int
evalLines = M.size . M.filter (>= 2) . getFreqs . (>>= linePoints)

-- Slightly faster implementation using a 1000x1000 array.
evalVec :: [Line] -> Int
evalVec lines = runST $ do
  a <- A.newArray ((0, 0), (1000, 1000)) 0 :: ST s (STUArray s (Int, Int) Int8)
  for_
    (lines >>= linePoints)
    (\(V2 x y) -> do
      v <- readArray a (x, y)
      writeArray a (x, y) (v + 1)
    )
  cs <- getElems a
  pure $ length . filter (>= 2) $ cs

day05a :: Solution [Line] Int
day05a = Solution { sParse = parse
                  , sShow  = show
                  , sSolve = Right . evalVec . filter isPerp
                  }

day05b :: Solution [Line] Int
day05b = Solution { sParse = parse, sShow = show, sSolve = Right . evalVec }
