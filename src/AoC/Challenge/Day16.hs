module AoC.Challenge.Day16
  ( day16a
  , day16b
  ) where

import           AoC.Solution
import           Control.DeepSeq                ( NFData )
import           Control.Monad.RWS              ( Product )
import           Data.Bifunctor                 ( first )
import           Data.Foldable                  ( foldl' )
import           Data.Functor.Foldable          ( cata )
import           Data.Functor.Foldable.TH       ( makeBaseFunctor )
import           GHC.Generics                   ( Generic )

import           Data.List                      ( splitAt )
import           Numeric                        ( readHex )

-- Convert an integer to a fixed number of binary digits.
toBinaryFixed :: Int -> Int -> Either String [Bool]
toBinaryFixed d x
  | d == 0 && x == 0 = Right []
  | d == 0 && x /= 0 = Left "Integer too large for the given fixed digits"
  | otherwise        = ((a /= 0) :) <$> toBinaryFixed (d - 1) b
  where (a, b) = x `divMod` (2 ^ (d - 1))

-- Convert a list of bits to an integer
binToInt :: [Bool] -> Int
binToInt = foldl' (\t b -> t * 2 + (if b then 1 else 0)) 0

-- Parse a string of hex characters to a list of bits (MSB first).
parseHexString :: String -> Either String [Bool]
parseHexString = fmap concat . traverse parseHexToBinary
 where
  parseHexToBinary :: Char -> Either String [Bool]
  parseHexToBinary x = case readHex (pure x) of
    (i, "") : rest -> toBinaryFixed 4 i
    _              -> Left "Unable to parse"

type Version = Int
data Packet = Operator Version Op [Packet]
            | Literal Version Int
  deriving (Show, Generic, NFData)

data Op = Sum
        | Product
        | Minimum
        | Maximum
        | GreaterThan
        | LessThan
        | Equals
  deriving (Show, Generic, NFData)

makeBaseFunctor ''Packet

parsePacket :: [Bool] -> Either String Packet
parsePacket bits = do
  (p, _, _) <- parsePacket' bits
  pure p

type Parser a = Either String (a, Int, [Bool])
-- Known length parser.
type KLParser a = Either String (a, [Bool])

parsePacket' :: [Bool] -> Parser Packet
parsePacket' bits = do
  (v, r) <- parseVersion bits
  (t, r) <- parseType r
  case t of
    Just op -> do
      (sub, opl, r) <- parseOperator r
      pure (Operator v op sub, 6 + opl, r)
    Nothing -> do
      (lit, ll, r) <- parseLiteral r
      pure (Literal v lit, 6 + ll, r)

 where
  parseVersion :: [Bool] -> KLParser Int
  parseVersion (a : b : c : rest) = Right (binToInt [a, b, c], rest)
  parseVersion x                  = Left $ "Invalid version: " <> show x

  parseType :: [Bool] -> KLParser (Maybe Op)
  parseType (a : b : c : rest) =
    let op = case binToInt [a, b, c] of
          0 -> Right $ Just Sum
          1 -> Right $ Just Product
          2 -> Right $ Just Minimum
          3 -> Right $ Just Maximum
          4 -> Right $ Nothing
          5 -> Right $ Just GreaterThan
          6 -> Right $ Just LessThan
          7 -> Right $ Just Equals
          x -> Left $ "Invalid operation: " <> show x
    in  fmap (, rest) op
  parseType x = Left $ "Invalid packet type: " <> show x

parseOperator :: [Bool] -> Parser [Packet]
parseOperator (lengthType : rest) = if lengthType
  then do
    (numSubPackets, r) <- parseInt 11 rest
    (subPackets, l, r) <- parseNPackets numSubPackets r
    pure (subPackets, l + 12, r)
  else do
    (numSubBits, r)    <- parseInt 15 rest
    (subPackets, l, r) <- parsePacketLength numSubBits r
    pure (subPackets, l + 16, r)

 where
  parseInt :: Int -> [Bool] -> KLParser Int
  parseInt x bits = if length bits < x
    then
      Left
      $  "Ran out of bits parsing integer of length "
      <> show x
      <> ": "
      <> show bits
    else Right . first binToInt . splitAt x $ bits

  parseNPackets :: Int -> [Bool] -> Parser [Packet]
  parseNPackets n bits = do
    (p, l, r) <- parsePacket' bits
    if n == 1
      then pure ([p], l, r)
      else do
        (p', l', r') <- parseNPackets (n - 1) r
        pure (p : p', l + l', r')

  parsePacketLength :: Int -> [Bool] -> Parser [Packet]
  parsePacketLength total bits = do
    (p, l, r) <- parsePacket' bits
    if total < l
      then
        Left
        $  "Wrong sub packet bit count: expected "
        <> show total
        <> ", got "
        <> show l
      else if total == l
        then pure ([p], l, r)
        else do
          (p', l', r') <- parsePacketLength (total - l) r
          pure (p : p', l + l', r')
parseOperator x = Left $ "Invalid operator packet: " <> show x

parseLiteral :: [Bool] -> Parser Int
parseLiteral bits = do
  (n, l, r) <- parseLiteralChunks bits
  pure (binToInt $ concat n, l, r)
 where
  parseLiteralChunks :: [Bool] -> Parser [[Bool]]
  parseLiteralChunks (i : a : b : c : d : rest)
    | not i = Right ([[a, b, c, d]], 5, rest)
    | otherwise =   (\(cs, l, r) -> ([a, b, c, d] : cs, l + 5, r))
    <$> parseLiteralChunks rest
  parseLiteralChunks x = Left $ "Invalid literal chunk: " <> show x

evalVersionSum :: PacketF Int -> Int
evalVersionSum = \case
  LiteralF v _      -> v
  OperatorF v _ sub -> v + sum sub

day16a :: Solution [[Bool]] Int
day16a = Solution
  { sParse = traverse parseHexString . lines
  , sShow  = show
  , sSolve = fmap (sum . fmap (cata evalVersionSum)) . traverse parsePacket
  }

evalPacketF :: PacketF Int -> Int
evalPacketF = \case
  OperatorF _ op v -> case op of
    Sum         -> sum v
    Product     -> product v
    Minimum     -> minimum v
    Maximum     -> maximum v
    GreaterThan -> if v !! 0 > v !! 1 then 1 else 0
    LessThan    -> if v !! 0 < v !! 1 then 1 else 0
    Equals      -> if v !! 0 == v !! 1 then 1 else 0
  LiteralF _ v -> v

day16b :: Solution [[Bool]] Int
day16b = Solution
  { sParse = traverse parseHexString . lines
  , sShow  = show
  , sSolve = fmap (head . fmap (cata evalPacketF)) . traverse parsePacket
  }
