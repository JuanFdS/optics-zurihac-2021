module Spec where

import Test.Hspec

correrTests = hspec $ do
    describe "" $ do
        it "" $ do
            1 + 1 `shouldBe` 2