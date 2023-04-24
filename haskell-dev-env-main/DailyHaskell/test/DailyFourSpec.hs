{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module DailyFourSpec where

import Test.Hspec
import DailyFour

main::IO ()
main = hspec spec 

spec :: Spec
spec = do 
    describe "zip3Lists" $ do 
        it "produces a list of tuples []" $
            (zip3Lists [][][]) `shouldBe` ([] :: [(Int, Int, Int)])
        it "produces a list of tuples [(1,7,4),(2,8,5),(3,9,6)]" $
            (zip3Lists [1,2,3][7,8,9][4,5,6]) `shouldBe` [(1,7,4),(2,8,5),(3,9,6)]
        it "produces a lsit of tuples [('a','s','e'),('b','t','f'),('c','r','g')]" $
            (zip3Lists ['a','b','c']['s','t','r']['e','f','g']) `shouldBe` [('a','s','e'),('b','t','f'),('c','r','g')]

    describe "unzipTriples" $ do    
        it "produces a tuple of 3 lists ([],[],[])" $
            (unzipTriples []) `shouldBe` (([],[],[]) :: ([Int], [Int], [Int]))  
        it "produces a tuple of 3 lists ([1,4,7],[2,5,8],[3,6,9])" $
            (unzipTriples [(1,2,3), (4,5,6), (7,8,9)]) `shouldBe` ([1,4,7],[2,5,8],[3,6,9])
        it "produces a tuple of 3 lists (['a','d',' g'],['b','e','h'],['c','f','i'])" $ 
            (unzipTriples [("a", "b", "c"), ("d", "e", "f"), ("g", "h", "i")]) `shouldBe` (["a","d","g"],["b","e","h"],["c","f","i"])
    describe "mergeSorted3" $ do    
        it "produces a sorted list []" $ 
           (mergeSorted3 [][][]) `shouldBe` ([] :: [Int])
        it "produces a sorted list [-1,0,1,2,3,4,5,8,10]" $ 
            (mergeSorted3 [2, 3, 5] [1, 8] [-1, 0, 4, 10]) `shouldBe`[-1,0,1,2,3,4,5,8,10]
        it "produces a sorted list [1,2,3,4,5,6,7,9,11]" $
            (mergeSorted3 [1, 3, 5] [2, 4, 6] [7, 9, 11]) `shouldBe` [1,2,3,4,5,6,7,9,11]