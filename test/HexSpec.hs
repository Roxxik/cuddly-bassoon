module HexSpec (main, spec) where

import Test.Hspec
import CuddlyBassoon.Crypto

import CuddlyBassoon.Hex

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "fromHex" $ do
        it "decodes an empty hexstring" $ do
            fromHex "" `shouldBe` Just []
        it "decodes a hexstring of 00" $ do
            fromHex "00" `shouldBe` Just [0x00]
        it "decodes a hexstring of ff" $ do
            fromHex "ff" `shouldBe` Just [0xff]
        it "decodes a hexstring of FF" $ do
            fromHex "FF" `shouldBe` Just [0xff]
        it "decodes a hexstring of 0123456789abcdef" $ do
            fromHex "0123456789abcdef" `shouldBe` Just [0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef]
    describe "fromHex failures" $ do
        it "fails to decode a hexstring of 0" $ do
            fromHex "0" `shouldBe` Nothing
        it "fails to decode a hexstring of xx" $ do
            fromHex "xx" `shouldBe` Nothing
    describe "toHex" $ do
        it "encodes an empty bytestring" $ do
            toHex [] `shouldBe` ""
        it "encodes a bytestring of [0x00]" $ do
            toHex [0x00] `shouldBe` "00"
        it "encodes a bytestring of [0xff]" $ do
            toHex [0xff] `shouldBe` "ff"
        it "encodes a bytestring of [0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef]" $ do
            toHex [0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef] `shouldBe` "0123456789abcdef"
