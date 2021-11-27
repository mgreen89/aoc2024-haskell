module AoC.Solution (
  Solution(..)
) where

data Solution a b = Solution
  { sParse :: String -> Either String a
  , sSolve :: a -> Either String b
  , sShow  :: b -> String
  }
