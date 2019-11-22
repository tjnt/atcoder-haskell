module SnippetSpec (spec) where

import Test.Hspec
import Snippet

spec :: Spec
spec = do
    describe "map2" $ do
        it "empty list" $
            map2 (+) [] `shouldBe` []
        it "one element" $
            map2 (+) [1] `shouldBe` []
        it "two element" $
            map2 (+) [1,2] `shouldBe` [3]
        it "three element" $
            map2 (+) [1,2,3] `shouldBe` [3]
        it "four element" $
            map2 (+) [1,2,3,4] `shouldBe` [3,7]

    describe "takeWhile2" $ do
        it "empty list" $
            takeWhile2 (<) ([] :: [Int]) `shouldBe` []
        it "one element" $
            takeWhile2 (<) [1] `shouldBe` [1]
        it "two element 1" $
            takeWhile2 (<) [1,2] `shouldBe` [1,2]
        it "two element 2" $
            takeWhile2 (<) [2,1] `shouldBe` [2]
        it "three element 1" $
            takeWhile2 (<) [1,2,3] `shouldBe` [1,2,3]
        it "three element 2" $
            takeWhile2 (<) [2,3,1] `shouldBe` [2,3]
        it "three element 3" $
            takeWhile2 (<) [3,2,1] `shouldBe` [3]
        it "many element" $
            takeWhile2 (<) [1,2,3,2,1] `shouldBe` [1,2,3]

    describe "dropWhile2" $ do
        it "empty list" $
            dropWhile2 (<) ([] :: [Int]) `shouldBe` []
        it "one element" $
            dropWhile2 (<) [1] `shouldBe` [1]
        it "two element 1" $
            dropWhile2 (<) [1,2] `shouldBe` [2]
        it "two element 2" $
            dropWhile2 (<) [2,1] `shouldBe` [2,1]
        it "three element 1" $
            dropWhile2 (<) [1,2,3] `shouldBe` [3]
        it "three element 2" $
            dropWhile2 (<) [2,3,1] `shouldBe` [3,1]
        it "three element 3" $
            dropWhile2 (<) [3,2,1] `shouldBe` [3,2,1]
        it "many element" $
            dropWhile2 (<) [1,2,3,2,1] `shouldBe` [3,2,1]
