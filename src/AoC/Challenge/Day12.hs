module AoC.Challenge.Day12
  ( day12a
  , day12b
  ) where

import           AoC.Solution
import           Control.Monad                  ( guard )
import           Data.Char                      ( isLower )
import           Data.List                      ( partition )
import           Data.List.Split                ( splitOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isNothing )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S

parse :: String -> Either String [(String, String)]
parse = traverse (listToTuple . splitOn "-") . lines
 where
  listToTuple [a, b] = Right (a, b)
  listToTuple x      = Left $ "Invalid input " <> show x

linkMap :: [(String, String)] -> Map String [String]
linkMap links = M.fromListWith (++) (concatMap getLinks links)
  where getLinks (a, b) = [(a, [b]), (b, [a])]

isSmall :: String -> Bool
isSmall = all isLower

findPaths :: [(String, String)] -> [[String]]
findPaths links = go S.empty "start"
 where
  ls = linkMap links
  go :: Set String -> String -> [[String]]
  go _    "end" = [["end"]]
  go seen pos   = do
    next <- ls M.! pos
    guard $ not $ isSmall next && S.member next seen
    (pos :) <$> go (S.insert pos seen) next

day12a :: Solution [(String, String)] Int
day12a =
  Solution { sParse = parse, sShow = show, sSolve = Right . length . findPaths }

findPaths' :: [(String, String)] -> [[String]]
findPaths' links = go S.empty Nothing "start"
 where
  ls = linkMap links
  go :: Set String -> Maybe String -> String -> [[String]]
  go _    _          "end" = [["end"]]
  go seen smallTwice pos   = do
    next          <- ls M.! pos
    newSmallTwice <- if isSmall next && S.member next seen
      then do
        guard $ isNothing smallTwice
        guard $ next /= "start"
        guard $ next /= "end"
        pure $ Just next
      else pure smallTwice
    (pos :) <$> go (S.insert pos seen) newSmallTwice next

day12b :: Solution [(String, String)] Int
day12b = Solution { sParse = parse
                  , sShow  = show
                  , sSolve = Right . length . findPaths'
                  }
