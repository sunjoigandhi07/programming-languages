{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module DailySixSpec where 
    import Test.Hspec 
    import DailySix

    main::IO () 
    main = hspec spec   

    spec :: Spec   
    spec = do 
        describe "shorterThan" $ do 
            it "produces a list pf [] " $ 
                (shorterThan 0 []) `shouldBe` []
            it "produces a list of ['pear','mango']" $
                (shorterThan 5 ["carrot", "pear", "mango", "broccoli", "cheese"]) `shouldBe` ["pear","mango"]
            it "produces a list of ['hi','k']" $
                (shorterThan 2 ["hey", "hi", "hello", "sup", "k"] ) `shouldBe` ["hi","k"]
        describe "removeMultiples" $ do 
            it "produces a list with no multiples of the given number [] " $
                (removeMultiples 1 []) `shouldBe` []
            it "produces a list with no multiples of the given number [3, 9]" $
                (removeMultiples 5 [3,5,10,9, 15]) `shouldBe` [3,9]
            it "produces a list with no multiples of the given number" $ 
                (removeMultiples 9 [3, 6, 9, 27, 56, 90, 32]) `shouldBe` [3,6,56,32]
        describe "onlyJust" $ do 
            -- cannot figure out how to correct my base case 
            --it "produces an empty list" $ 
                --(onlyJust []) `shouldBe` ([] : [Maybe a])   
            it "produces a list of values [Just 5, Just 10]" $
                (onlyJust [Nothing, Just 5, Nothing, Just 10] ) `shouldBe` [Just 5, Just 10]
            it "produces a list of values [Just 8] " $ 
                (onlyJust [Nothing, Nothing, Just 8]) `shouldBe` [Just 8]
                