module CuddlyBassoon.Crypto where

import GHC.Word (Word8)
import Crypto.Hash
import Crypto.Hash.Algorithms (Keccak_256)
import qualified Data.ByteArray as BA (Bytes, pack, unpack)

keccak256 :: [Word8] -> [Word8]
keccak256 x =
    let ba = BA.pack x :: BA.Bytes in
    let d = hash ba :: Digest Keccak_256 in
    BA.unpack d

--keccak256bytes :: [Word8] -> Bytes32
--keccak256bytes = Bytes32 . getBe . keccak256
