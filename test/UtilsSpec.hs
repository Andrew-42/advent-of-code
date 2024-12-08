{-# OPTIONS_GHC -Wno-type-defaults #-}

module UtilsSpec where

import qualified Data.Map as M
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)
import Utils (freq, transpose)

utilsSpec :: Spec
utilsSpec = do
    transposeTest
    freqTest

transposeTest :: SpecWith ()
transposeTest = do
    describe "Test transposing a list." $ do
        it "Transpose a list of lists" $ do
            transpose [[1], [3], [5]] `shouldBe` [[1, 3, 5]]

freqTest :: SpecWith ()
freqTest = do
    describe "Test calculating frequencies." $ do
        it "Get char frequencies" $ do
            let expectedFreq =
                    M.fromList
                        [ ('a', 2)
                        , ('b', 1)
                        , ('c', 1)
                        , ('d', 3)
                        , ('e', 3)
                        , ('f', 1)
                        , ('g', 2)
                        , ('h', 1)
                        ]
            freq "aabcdddeeefggh" `shouldBe` expectedFreq
