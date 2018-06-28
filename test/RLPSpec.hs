module RLPSpec (main, spec) where

import Test.Hspec
import RLP

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "RLP arrays" $ do
        it "decodes an empty RLP byte array" $ do
            getRlp [0x80] `shouldBe` Just (Leaf [])
        it "decodes a scalar 0x00" $ do
            getRlp [0x00] `shouldBe` Just (Leaf [0x00])
        it "decodes a scalar 0x7f" $ do
            getRlp [0x7f] `shouldBe` Just (Leaf [0x7f])
        it "decodes a scalar 0x80" $ do
            getRlp [0x81, 0x80] `shouldBe` Just (Leaf [0x80])
        it "decodes a scalar 0xff" $ do
            getRlp [0x81, 0xff] `shouldBe` Just (Leaf [0xff])
        it "decodes a scalar with 2 bytes" $ do
            getRlp [0x82, 0x00, 0x00] `shouldBe` Just (Leaf [0x00, 0x00])
        it "decodes a scalar with 55 bytes" $ do
            getRlp (0xb7 : replicate 55 0x00) `shouldBe` Just (Leaf (replicate 55 0x00))
        it "decodes a scalar with 56 bytes" $ do
            getRlp (0xb8 : 56 : replicate 56 0x00) `shouldBe` Just (Leaf (replicate 56 0x00))

    describe "RLP array failures" $ do
        it "fails to decode an array of length 0 given length 1" $ do
            getRlp [0x81] `shouldBe` Nothing
        it "fails to decode an array of length 1 given length 2" $ do
            getRlp [0x82, 0x00] `shouldBe` Nothing
        it "fails to decode an array of length 2 given length 1" $ do
            getRlp [0x81, 0x00, 0x00] `shouldBe` Nothing
        it "fails to decode an array of length 1 with a length encoding of >55" $ do
            getRlp [0xb8, 0x01, 0x00] `shouldBe` Nothing
        it "fails to decode an array of length 56 where the length has an extra 0x00" $ do
            getRlp ([0xb9, 0x00, 56] ++ replicate 56 0x00) `shouldBe` Nothing

    describe "RLP sequence" $ do
        it "decodes an empty RLP sequence " $ do
            getRlp [0xc0] `shouldBe` Just (Node [])
        it "decodes a RLP sequence of 1 scalar 0" $ do
            getRlp [0xc1, 0x00] `shouldBe` Just (Node [Leaf [0x00]])
        it "decodes a RLP sequence of 55 scalar 0" $ do
            getRlp (0xf7 : replicate 55 0x00) `shouldBe` Just (Node (replicate 55 (Leaf [0x00])))
        it "decodes a RLP sequence of 56 scalar 0" $ do
            getRlp (0xf8 : 56 : replicate 56 0x00) `shouldBe` Just (Node (replicate 56 (Leaf [0x00])))
        it "decodes a RLP sequence of 1 scalar with 54 bytes" $ do
            getRlp (0xf7 : 0xb6 : replicate 54 0x00) `shouldBe` Just (Node [Leaf (replicate 54 0x00)])
        it "decodes a RLP sequence of 1 scalar with 55 bytes" $ do
            getRlp (0xf8 : 56 : 0xb7 : replicate 55 0x00) `shouldBe` Just (Node [Leaf (replicate 55 0x00)])
        it "decodes a RLP sequence of 1 scalar with 56 bytes" $ do
            getRlp (0xf8 : 58 : 0xb8 : 56 : replicate 56 0x00) `shouldBe` Just (Node [Leaf (replicate 56 0x00)])
        it "decodes a RLP sequence of 1 RLP sequence of 1 scalar 0" $ do
            getRlp [0xc2, 0xc1, 0x00] `shouldBe` Just (Node [Node [Leaf [0x00]]])

    describe "RLP sequence failures" $ do
        it "fails to decode an empty RLP sequence with extra bytes" $ do
            getRlp [0xc0, 0x00] `shouldBe` Nothing
        it "fails to decode a RLP sequence of 2 scalar 0 given length 1" $ do
            getRlp [0xc1, 0x00, 0x00] `shouldBe` Nothing
        it "fails to decode a RLP sequence of 1 scalar 0 given length 2" $ do
            getRlp [0xc2, 0x00] `shouldBe` Nothing
        it "fails to decode a RLP sequence of length 1 with a length encoding of >55" $ do
            getRlp [0xf8, 0x01, 0x00] `shouldBe` Nothing
        it "fails to decode a RLP sequence of length 56 where the length has an extra 0x00" $ do
            getRlp (0xf9 : 0x00 : 56 : 0xb7 : replicate 55 0x00) `shouldBe` Nothing
