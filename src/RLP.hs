module RLP where

import GHC.Word (Word8)
import Data.Bits (Bits, (.&.), (.|.), shiftR, shiftL)
import Data.FixedPoint (Word256)
import Control.Applicative (Alternative(..), many, empty)

data Tree a = Leaf a | Node [Tree a]
    deriving (Eq, Show)

type RLPTree = Tree [Word8]

-- A parser is a function taking a list of bytes and producing a result and a remaining list of bytes. This can fail
newtype Parser a = Parser { runParser :: [Word8] -> Maybe (a, [Word8]) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \i -> case p i of
        Just (o, i') -> Just (f o, i')
        Nothing -> Nothing

instance Applicative Parser where
    pure o = Parser $ \i -> Just (o, i)
    Parser pf <*> Parser px = Parser $ \i -> case pf i of
        Nothing -> Nothing
        Just (f, i') -> case px i' of
            Nothing -> Nothing
            Just (x, i'') -> Just (f x, i'')

instance Alternative Parser where
    empty = Parser $ \i -> Nothing
    Parser p <|> Parser q = Parser $ \i -> case p i of
        Just (o, i') -> Just (o, i')
        Nothing -> q i

instance Monad Parser where
    return = pure
    Parser k >>= f = Parser $ \i -> case k i of
        Nothing -> Nothing
        Just (o, i') -> runParser (f o) i'


-- parsing:
parse :: Parser a -> [Word8] -> Maybe a
parse p i = fst <$> runParser (final p) i

final :: Parser a -> Parser a
final p = p <* eoi

-- basic parsers:
item :: Parser Word8
item = Parser $ \i -> case i of
    [] -> Nothing
    (o:i') -> Just (o, i')

eoi :: Parser ()
eoi = Parser $ \i -> case i of
    [] -> Just ((), [])
    _ -> Nothing

-- combinators:
manyN :: Integral n => Parser a -> n -> Parser [a]
manyN _ 0 = return []
manyN p n = do
    x <- p
    xs <- manyN p (n - 1)
    return (x:xs)

force :: Parser (Maybe a) -> Parser a
force (Parser p) = Parser $ \i -> case p i of
    Just ((Just o), i') -> Just (o, i')
    _ -> Nothing

-- generic conversion functions
getLe :: (Integral a, Bits a) => [Word8] -> a
getLe [] = 0
getLe (x:xs) = getLe xs `shiftL` 8 .|. fromIntegral x

-- no following zeros allowed
getLeShortest :: (Integral a, Bits a) => [Word8] -> Maybe a
getLeShortest [] = Just 0
getLeShortest [0] = Nothing
getLeShortest (x:xs) = do
    b <- getLeShortest xs
    return (b `shiftL` 8 .|. fromIntegral x)

getBe :: (Integral a, Bits a) => [Word8] -> a
getBe = getLe . reverse

--no starting zeros allowed
getBeShortest :: (Integral a, Bits a) => [Word8] -> Maybe a
getBeShortest = getLeShortest . reverse

-- concrete parsers that are needed:
bytes :: Integral a => a -> Parser [Word8]
bytes = manyN item

parseLength :: Word8 -> Parser Word256
parseLength n = force (getBeShortest <$> bytes n)

trees :: Integral a => a -> Parser [RLPTree]
trees n = force (parse (many tree) <$> bytes n)

tree :: Parser RLPTree
tree = do
    i <- item
    case i of
        n | n <  0x80             -> return (Leaf [n])
        n | n >= 0x80 && n < 0xb8 -> Leaf <$> bytes (n - 0x80)
        n | n >= 0xb8 && n < 0xc0 -> do
            l <- parseLength (n - 0xb7)
            if l < 0x38 then empty else Leaf <$> bytes l
        n | n >= 0xc0 && n < 0xf8 -> Node <$> trees (n - 0xc0)
        n | n >= 0xf8             -> do
            l <- parseLength (n - 0xf7)
            if l < 0x38 then empty else Node <$> trees l

getRlp :: [Word8] -> Maybe RLPTree
getRlp = parse tree

putBe :: (Integral a, Bits a) => a -> [Word8]
putBe = reverse . putLe

putLe :: (Integral a, Bits a) => a -> [Word8]
putLe 0 = []
putLe x
    | x < 256   = [fromIntegral x]
    | otherwise = (fromIntegral x .&. 0xFF) : putLe (x `shiftR` 8)

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
