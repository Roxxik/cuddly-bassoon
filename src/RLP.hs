module RLP where

data Tree a = Leaf a | Node [Tree a]

type RLPTree = Tree [Word8]
