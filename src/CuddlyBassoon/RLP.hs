module CuddlyBassoon.RLP (Tree(..), RLPTree, putRlp, getRlp) where

import CuddlyBassoon.Convert (putBe, getBeShortest)
import CuddlyBassoon.RLP.Parser (Parser, parse, item, force, bytes)

import Control.Applicative (empty, many)
import GHC.Word (Word8)

data Tree a = Leaf a | Node [Tree a]
    deriving (Eq, Show)

type RLPTree = Tree [Word8]

putRlp :: RLPTree -> [Word8]
putRlp (Leaf xs)
    | l == 1 && x < 128 = [x]
    | l < 56            = [128 + fromIntegral l] ++ xs
    | otherwise         = [183 + fromIntegral lb] ++ b ++ xs
    where
        l = length xs
        x = head xs
        b = putBe l
        lb = length b
putRlp (Node xs)
    | ls < 56   = [192 + fromIntegral ls] ++ s
    | otherwise = [247 + fromIntegral lb] ++ b ++ s
    where
        s = concatMap putRlp xs
        ls = length s
        b = putBe ls
        lb = length b

getRlp :: [Word8] -> Maybe RLPTree
getRlp = parse tree

length_shortest :: Word8 -> Parser Integer
length_shortest n = force (getBeShortest <$> bytes n)

trees :: Integral a => a -> Parser [RLPTree]
trees n = force (parse (many tree) <$> bytes n)

tree :: Parser RLPTree
tree = do
    i <- item
    case i of
        n | n <  0x80             -> return (Leaf [n])
        n | n >= 0x80 && n < 0xb8 -> Leaf <$> bytes (n - 0x80)
        n | n >= 0xb8 && n < 0xc0 -> do
            l <- length_shortest (n - 0xb7)
            if l < 0x38 then empty else Leaf <$> bytes l
        n | n >= 0xc0 && n < 0xf8 -> Node <$> trees (n - 0xc0)
        n | n >= 0xf8             -> do
            l <- length_shortest (n - 0xf7)
            if l < 0x38 then empty else Node <$> trees l
