module Spec where

import Test.Hspec
import Exercises
import Optics.Core

correrTests = hspec $ do
    describe "anyOlderThan" $ do
        it "given an age, it's false for a person younger than that age that has no pets" $ do
           anyOlderThan 66 alice `shouldBe` False
        it "given an age, it's true for a person older than that age that has no pets" $ do
           anyOlderThan 50 alice `shouldBe` True
        it "given an age, it's true for a person younger than that age with a pet older than it" $ do
           anyOlderThan 2 pepe `shouldBe` True
        it "given an age, it's false for a person younger than that age with a pet also younger than it" $ do
           anyOlderThan 10 pepe `shouldBe` False
    describe "anyOlderThan' defined with has" $ do
        it "works the same as anyOlderThan" $ do
            anyOlderThan' 66 alice `shouldBe` anyOlderThan 66 alice
            anyOlderThan' 50 alice `shouldBe` anyOlderThan 50 alice
            anyOlderThan' 2 pepe `shouldBe` anyOlderThan 2 pepe
            anyOlderThan' 4 pepe `shouldBe` anyOlderThan 4 pepe
    describe "petsOlderThan" $ do
        it "returns a fold with the pets from a person older than a certain age" $ do
            toListOf (petsOlderThan 4) pepe `shouldBe` [MkPet { _petName = "nala", _petAge = 5 }]