{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module AoC.Challenge.Day14
  ( day14a
  -- , day14b
  ) where

import           AoC.Solution
import           AoC.Util                       ( getFreqs )
import           Data.Foldable                  ( foldl' )
import           Data.List.Split                ( splitOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M

listTo2Tuple :: [a] -> Either String (a, a)
listTo2Tuple [a, b] = Right (a, b)
listTo2Tuple _      = Left "Not a 2-elem list"

parse :: String -> Either String (String, [(String, Char)])
parse inp = do
  (template, inserts) <- listTo2Tuple . splitOn "\n\n" $ inp
  insList <- traverse (listTo2Tuple . splitOn " -> ") . lines $ inserts
  pure (template, fmap (fmap head) insList)

polyStep :: Map String Char -> String -> String
polyStep m = reverse . go []
 where
  go :: String -> String -> String
  go out []             = out
  go out [a           ] = a : out
  go out (a : b : rest) = go (m M.! [a, b] : a : out) (b : rest)

partA :: (String, [(String, Char)]) -> Int
partA (tmpl, ins) =
  let insMap     = M.fromList ins
      fullChain  = (!! 10) . iterate (polyStep insMap) $ tmpl
      fullCounts = getFreqs fullChain
  in  maximum (M.elems fullCounts) - minimum (M.elems fullCounts)

day14a :: Solution (String, [(String, Char)]) Int
day14a = Solution { sParse = parse, sShow = show, sSolve = Right . partA }

day14b :: Solution _ _
day14b = Solution { sParse = Right, sShow = show, sSolve = Right }
