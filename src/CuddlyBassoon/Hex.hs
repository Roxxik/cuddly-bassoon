module CuddlyBassoon.Hex where

import Data.Maybe
import GHC.Word (Word8)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C

toHex :: [Word8] -> String
toHex = concatMap w
    where
        s = "0123456789abcdef"
        w ch = [s !! div x 16,s !! mod x 16]
            where x = fromEnum ch

fromHex' :: String -> [Word8]
fromHex' = fromJust . fromHex
fromHex :: String -> Maybe [Word8]
fromHex [] = Just []
fromHex (a:b:r) = do
    x <- c a
    y <- c b
    xs <- fromHex r
    return (toEnum ((x * 16) + y) : xs)
fromHex _ = Nothing


c :: Char -> Maybe Int
c '0' = Just 0
c '1' = Just 1
c '2' = Just 2
c '3' = Just 3
c '4' = Just 4
c '5' = Just 5
c '6' = Just 6
c '7' = Just 7
c '8' = Just 8
c '9' = Just 9
c 'A' = Just 10
c 'B' = Just 11
c 'C' = Just 12
c 'D' = Just 13
c 'E' = Just 14
c 'F' = Just 15
c 'a' = Just 10
c 'b' = Just 11
c 'c' = Just 12
c 'd' = Just 13
c 'e' = Just 14
c 'f' = Just 15
c _   = Nothing
