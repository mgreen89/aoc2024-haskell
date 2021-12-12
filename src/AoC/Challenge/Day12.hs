{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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
  go _ "end" = [["end"]]
  go seen pos =
    let nexts    = ls M.! pos
        filtered = filter (\n -> not (isSmall n && S.member n seen)) nexts
    in  fmap (pos :) $ filtered >>= go (S.insert pos seen)

day12a :: Solution [(String, String)] Int
day12a =
  Solution { sParse = parse, sShow = show, sSolve = Right . length . findPaths }

findPaths' :: [(String, String)] -> [[String]]
findPaths' links = go S.empty Nothing "start"
 where
  ls = linkMap links
  go :: Set String -> Maybe String -> String -> [[String]]
  go _ _ "end" = [["end"]]
  go seen smallTwice pos =
    let
      nexts = ls M.! pos
      (good, seenSmall) =
        partition (\n -> not (isSmall n && S.member n seen)) nexts
      seenSmallFilter =
        filter (\s -> not (s == "start" || s == "end")) seenSmall
      goodRest      = good >>= go (S.insert pos seen) smallTwice
      seenSmallRest = case smallTwice of
        Nothing ->
          seenSmallFilter >>= (\s -> go (S.insert pos seen) (Just s) s)
        Just _ -> []
    in
      fmap (pos :) (goodRest ++ seenSmallRest)

day12b :: Solution _ _
day12b = Solution { sParse = parse
                  , sShow  = show
                  , sSolve = Right . length . findPaths'
                  }
