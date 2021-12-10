module AoC.Challenge.Day10
  ( day10a
  , day10b
  ) where

import           AoC.Solution
import           Data.Either                    ( lefts
                                                , rights
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.List                      ( sort )

parenCorruptScore :: Char -> Int
parenCorruptScore ')' = 3
parenCorruptScore ']' = 57
parenCorruptScore '}' = 1197
parenCorruptScore '>' = 25137
parenCorruptScore x   = error $ "Invalid paren corrupt score: " <> pure x

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

-- Return Either (Left corruptScore) (Right stack)
checkLine :: String -> Either Int [Char]
checkLine = go []
 where
  go stack cs = case cs of
    (c : rest) -> case checkChar stack c of
      Left  i   -> Left $ parenCorruptScore i
      Right ci' -> go ci' rest
    _ -> Right stack

day10a :: Solution [String] Int
day10a = Solution { sParse = Right . lines
                  , sShow  = show
                  , sSolve = Right . sum . lefts . fmap checkLine
                  }

parenCompleteScore :: Char -> Int
parenCompleteScore ')' = 1
parenCompleteScore ']' = 2
parenCompleteScore '}' = 3
parenCompleteScore '>' = 4
parenCompleteScore x   = error $ "Invalid paren complete score: " <> pure x

-- Return Either (Left corruptScore) (Right completeScore)
completeLine :: String -> Either Int Int
completeLine = fmap complete . checkLine
 where
  complete = foldl' go 0
    where go tot x = (tot * 5) + parenCompleteScore (other x)

-- Find the median (from day 7).
-- This assumes there's an odd number in the list!
median :: [Int] -> Int
median xs =
  let s = sort xs
      l = length s
  in  (s !! (l `div` 2))

day10b :: Solution [String] Int
day10b = Solution { sParse = Right . lines
                  , sShow  = show
                  , sSolve = Right . median . rights . fmap completeLine
                  }
