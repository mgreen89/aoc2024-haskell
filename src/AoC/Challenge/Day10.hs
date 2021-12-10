module AoC.Challenge.Day10
  ( day10a
  , day10b
  ) where

import           AoC.Solution
import           Data.Foldable                  ( foldl' )
import           Data.List                      ( sort )
import           Data.Maybe                     ( mapMaybe )

parenToScore :: Char -> Int
parenToScore ')' = 3
parenToScore ']' = 57
parenToScore '}' = 1197
parenToScore '>' = 25137
parenToScore x   = error $ "Invalid paren score: " <> pure x

other :: Char -> Char
other '(' = ')'
other ')' = '('
other '[' = ']'
other ']' = '['
other '{' = '}'
other '}' = '{'
other '<' = '>'
other '>' = '<'
other x   = error $ "Invalid paren: " <> pure x

checkChar :: [Char] -> Char -> Either Char [Char]
checkChar stack c = if isOpen c
  then Right $ c : stack
  else case stack of
    c' : rest | c' == other c -> Right rest
    _                         -> Left c
  where isOpen = flip elem ['(', '[', '{', '<']

checkLine :: String -> Maybe Int
checkLine = go []
 where
  go stack cs = case cs of
    (c : rest) -> case checkChar stack c of
      Left  i   -> Just $ parenToScore i
      Right ci' -> go ci' rest
    _ -> Nothing

day10a :: Solution [String] Int
day10a = Solution { sParse = Right . lines
                  , sShow  = show
                  , sSolve = Right . sum . mapMaybe checkLine
                  }

parenCompleteScore :: Char -> Int
parenCompleteScore ')' = 1
parenCompleteScore ']' = 2
parenCompleteScore '}' = 3
parenCompleteScore '>' = 4
parenCompleteScore x   = error $ "Invalid paren: " <> pure x

completeLine :: String -> Maybe Int
completeLine = fmap complete . check []
 where
  check stack cs = case cs of
    (c : rest) -> case checkChar stack c of
      Left  _   -> Nothing
      Right ci' -> check ci' rest
    _ -> Just stack

  complete = foldl' go 0
    where go tot x = (tot * 5) + parenCompleteScore (other x)

-- Find the median (from day 7).
-- This assumes there's an odd number in the list!
median :: [Int] -> Int
median xs =
  let s      = sort xs
      l      = length s
  in  (s !! (l `div` 2))

day10b :: Solution [String] Int
day10b = Solution { sParse = Right . lines
                  , sShow  = show
                  , sSolve = Right . median . mapMaybe completeLine
                  }
