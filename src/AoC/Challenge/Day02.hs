module AoC.Challenge.Day02
  ( day02a
  , day02b
  ) where

import           AoC.Solution
import           Control.DeepSeq
import           Data.Bifunctor
import           Data.Foldable
import           Data.Void
import           GHC.Generics
import           Linear.V2                      ( V2(..) )
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MP
import qualified Text.Megaparsec.Char.Lexer    as MP

data Instruction = Forward Int
                 | Down Int
                 | Up Int
  deriving (Show, Generic, NFData)

instructionParser :: MP.Parsec Void String Instruction
instructionParser = do
  dir <- MP.choice
    [ Forward <$ MP.string "forward"
    , Down <$ MP.string "down"
    , Up <$ MP.string "up"
    ]
  MP.space1
  dir <$> MP.decimal

instructionsParser :: MP.Parsec Void String [Instruction]
instructionsParser = MP.sepEndBy instructionParser MP.eol

parseInstructions :: String -> Either String [Instruction]
parseInstructions =
  first MP.errorBundlePretty . MP.parse instructionsParser "day02"

instrToVecA :: Instruction -> V2 Int
instrToVecA instr = case instr of
  Forward x -> V2 x 0
  Down    x -> V2 0 x
  Up      x -> V2 0 (-x)

day02a :: Solution [Instruction] Int
day02a = Solution { sParse = parseInstructions
                  , sShow  = show
                  , sSolve = Right . product . sum . fmap instrToVecA
                  }

day02bf :: (Int, V2 Int) -> Instruction -> (Int, V2 Int)
day02bf (aim, pos) inst = case inst of
  Forward x -> (aim, pos + V2 x (x * aim))
  Down    x -> (aim + x, pos)
  Up      x -> (aim - x, pos)

day02b :: Solution [Instruction] Int
day02b = Solution { sParse = parseInstructions
                  , sShow  = show
                  , sSolve = Right . product . snd . foldl' day02bf (0, V2 0 0)
                  }
