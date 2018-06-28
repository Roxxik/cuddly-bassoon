module CuddlyBassoon.RLP.Parser where

import GHC.Word (Word8)
import Control.Applicative (Alternative(..))

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

bytes :: Integral a => a -> Parser [Word8]
bytes = manyN item
