module CryptoSpec (main, spec) where

import Test.Hspec
import CuddlyBassoon.Crypto

import CuddlyBassoon.Hex

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "hash" $ do
        it "hashes a bytearray of length 0" $ do
            keccak256 [] `shouldBe` fromHex' "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
        it "hashes a bytearray of [0x20]" $ do
            keccak256 [0x20] `shouldBe` fromHex' "681afa780d17da29203322b473d3f210a7d621259a4e6ce9e403f5a266ff719a"
