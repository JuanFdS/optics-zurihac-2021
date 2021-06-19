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
    describe "petNamesOlderThan" $ do
        it "returns a fold with the pet names from a person older than a certain age" $ do
            petNamesOlderThan 4 pepe `shouldBe` ["nala"]
    describe "names" $ do
        it "returns the name of the person along with the names of their pets" $ do
            toListOf names pepe `shouldBe` ["Pepe", "pepita", "nala"]
    describe "capitaliseNames" $ do
        it "returns the name of the person along with the names of their pets, capitalised" $ do
            capitaliseNames bob `shouldBe` MkPerson { _personName = "BOB"
               , _personAge  = 42
               , _personPets = [ MkPet { _petName = "MR SCRUFFY", _petAge = 3 } ]
               }
    describe "leaves" $ do
        it "traverses through the leaf labels of a tree" $ do
            toListOf leaves treeExample  `shouldBe` [3, 2, 4, 1, 2]
    describe "preorder" $ do
        it "traverses through the nodes in pre-order" $ do
            toListOf preorder treeExample `shouldBe` ['b', 'a', 'c', 'd']
    describe "attachIndices" $ do
        it "traverses through the nodes in pre-order" $ do
            attachIndices leaves treeExample `shouldBe` Node (Node (Node (Leaf (0, 3))
                            'c'
                            (Leaf (1, 2)))
                    'a'
                    (Leaf (2, 4)))
                'b'
                (Node (Leaf (3, 1))
                    'd'
                    (Leaf (4, 2)))

    describe "dubiousLens" $ do
        it "breaks the law (?" $ do
            over dubiousLens id (MkDubious 5 "hi") `shouldBe` MkDubious 6 "hi" 

    describe "traverseDuplicated" $ do
        it "visits the value in Duplicated twice" $ do
            toListOf traverseDuplicated (MkDuplicated 5) `shouldBe` [5, 5] 