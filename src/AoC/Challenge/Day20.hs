module AoC.Challenge.Day20
  ( day20a
  , day20b
  ) where

import           AoC.Solution
import           AoC.Util                       ( Point
                                                , listTo2Tuple
                                                )
import           Data.Array                     ( Array )
import qualified Data.Array                    as A
import           Data.Bifunctor                 ( bimap )
import           Data.Foldable                  ( foldl' )
import           Data.List.Split                ( splitOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Linear.V2                      ( V2(..) )

parseIEA :: String -> Array Int Bool
parseIEA s = A.array (0, length s - 1) (zip [0 ..] . fmap (== '#') $ s)

parseImage :: String -> Map Point Bool
parseImage =
  M.fromList
    . concat
    . zipWith (\y -> zipWith (\x c -> (V2 x y, c == '#')) [0 ..]) [0 ..]
    . lines

parse :: String -> Either String (Array Int Bool, Map Point Bool)
parse = fmap (bimap parseIEA parseImage) . listTo2Tuple . splitOn "\n\n"

boolsToInt :: [Bool] -> Int
boolsToInt = foldl' go 0 where go a x = 2 * a + (if x then 1 else 0)

enhancePixel
  :: Array Int Bool -> Map Point Bool -> Int -> Point -> (Point, Bool)
enhancePixel iea i iter p =
  let neighbdeltas = [ V2 x y | y <- [-1 .. 1], x <- [-1 .. 1] ]
      neighbs      = fmap (+ p) neighbdeltas
      idxBits      = fmap (\n -> M.findWithDefault (odd iter) n i) neighbs
      idx          = boolsToInt idxBits
  in  (p, iea A.! idx)

-- Run a single enhancement step over an entire image.
enhance :: Array Int Bool -> Map Point Bool -> Int -> Map Point Bool
enhance iea i iter =
  let (xs, ys) = unzip . fmap (\(V2 x y, _) -> (x, y)) . M.toList $ i
      xMin     = minimum xs - 2
      xMax     = maximum xs + 2
      yMin     = minimum ys - 2
      yMax     = maximum ys + 2
  in  M.fromList
        . fmap (enhancePixel iea i iter . uncurry V2)
        $ [ (x, y) | y <- [yMin .. yMax], x <- [xMin .. xMax] ]

countLitEnhanced :: Int -> Array Int Bool -> Map Point Bool -> Int
countLitEnhanced n iea i0 =
  M.size . M.filter id . foldl' (enhance iea) i0 $ [0..(n-1)]

day20a :: Solution (Array Int Bool, Map Point Bool) Int
day20a = Solution
  { sParse = parse
  , sShow  = show
  , sSolve = Right . uncurry (countLitEnhanced 2)
  }

day20b :: Solution (Array Int Bool, Map Point Bool) Int
day20b = Solution
  { sParse = parse
  , sShow  = show
  , sSolve = Right . uncurry (countLitEnhanced 50)
  }
