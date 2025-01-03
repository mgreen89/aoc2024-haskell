module AoC.Challenge.Day17 (
  day17a,
  day17b,
)
where

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Array (Array)
import qualified Data.Array as A
import qualified Data.Array.IArray as IA
import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, (.&.), (.^.))
import Data.List (intercalate)
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

data Inst
  = Adv Int
  | Bxl Int
  | Bst Int
  | Jnz Int
  | Bxc Int
  | Out Int
  | Bdv Int
  | Cdv Int
  deriving (Eq, Generic, NFData, Show)

parseInsts :: [Int] -> [Inst]
parseInsts (a : b : rest) =
  ( case a of
      0 -> Adv
      1 -> Bxl
      2 -> Bst
      3 -> Jnz
      4 -> Bxc
      5 -> Out
      6 -> Bdv
      7 -> Cdv
      x -> error ("invalid instruction op: " ++ show x)
  )
    b
    : parseInsts rest
parseInsts [] = []
parseInsts _ = error "Invalid instructions"

data ComputerState = CS
  { inst_ptr :: Int
  , a :: Int
  , b :: Int
  , c :: Int
  , outs :: [Int]
  }
  deriving (Eq, Generic, NFData, Show)

parser :: MP.Parsec Void String (ComputerState, Array Int Inst, [Int])
parser = do
  a <- MP.string "Register A: " *> MPL.decimal <* MP.newline
  b <- MP.string "Register B: " *> MPL.decimal <* MP.newline
  c <- MP.string "Register C: " *> MPL.decimal <* MP.newline
  MP.newline
  MP.string "Program: "
  insts <- MP.sepBy MPL.decimal (MP.char ',')
  let parsed = parseInsts insts
  pure (CS 0 a b c [], A.listArray (0, length parsed - 1) parsed, insts)

parse :: String -> Either String (ComputerState, Array Int Inst, [Int])
parse =
  first MP.errorBundlePretty . MP.parse parser "day17"

comboVal :: ComputerState -> Int -> Int
comboVal _ 0 = 0
comboVal _ 1 = 1
comboVal _ 2 = 2
comboVal _ 3 = 3
comboVal cs 4 = cs.a
comboVal cs 5 = cs.b
comboVal cs 6 = cs.c
comboVal _ 7 = error "Invalid legacy combo operand!"
comboVal _ _ = error "Invalid combo operand!"

step :: Inst -> ComputerState -> ComputerState
step i cs = (\s -> s{inst_ptr = s.inst_ptr + 1}) $ case i of
  Adv x -> cs{a = cs.a `shiftR` comboVal cs x}
  Bxl x -> cs{b = cs.b .^. x}
  Bst x -> cs{b = comboVal cs x .&. 7}
  Jnz x -> if cs.a == 0 then cs else cs{inst_ptr = x - 1}
  Bxc _ -> cs{b = cs.b .^. cs.c}
  Out x -> cs{outs = (comboVal cs x .&. 7) : cs.outs}
  Bdv x -> cs{b = cs.a `shiftR` comboVal cs x}
  Cdv x -> cs{c = cs.a `shiftR` comboVal cs x}

solveA :: (ComputerState, Array Int Inst, [Int]) -> [Int]
solveA (initCs, instrs, _) = reverse . outs $ go initCs
 where
  go :: ComputerState -> ComputerState
  go cs = case instrs IA.!? cs.inst_ptr of
    Just i -> go (step i cs)
    Nothing -> cs

day17a :: Solution (ComputerState, Array Int Inst, [Int]) [Int]
day17a =
  Solution
    { sParse = parse
    , sShow = intercalate "," . fmap show
    , sSolve = Right . solveA
    }

-- Build up a passing number by generating the output numbers one-by-one
-- after inspecting the program manually.
solveB :: (ComputerState, Array Int Inst, [Int]) -> Int
solveB (_, _, rawProgram) =
  head $ go 0 (reverse rawProgram)
 where
  go :: Int -> [Int] -> [Int]
  go n [] = [n]
  go n (tgt : tgts) =
    [ x
    | i <- [0 .. 7]
    , let t = (n `shiftL` 3) + i
    , check tgt t
    , x <- go t tgts
    ]

  check :: Int -> Int -> Bool
  check tgt n =
    -- This is manually generated from inspection of the program.
    let
      b = (n .&. 7) .^. 2
      c = n `shiftR` b
     in
      (b .^. c .^. 7) .&. 7 == tgt

day17b :: Solution (ComputerState, Array Int Inst, [Int]) Int
day17b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB}
