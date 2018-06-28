module CuddlyBassoon.Convert where

import GHC.Word (Word8)
import Data.Bits (Bits, (.&.), (.|.), shiftR, shiftL)

putBe :: (Integral a, Bits a) => a -> [Word8]
putBe = reverse . putLe

putLe :: (Integral a, Bits a) => a -> [Word8]
putLe 0 = []
putLe x
    | x < 256   = [fromIntegral x]
    | otherwise = (fromIntegral x .&. 0xFF) : putLe (x `shiftR` 8)

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
