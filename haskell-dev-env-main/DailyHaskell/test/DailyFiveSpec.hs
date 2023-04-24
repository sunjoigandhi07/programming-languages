{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module DailyFiveSpec where 
    import Test.Hspec 
    import DailyFive 

    main::IO () 
    main = hspec spec   

    spec :: Spec   
    spec = do 
        describe "multPairs" $ do 
            it "produces a product of 2" $
                (multPairs [(2, 2)]) `shouldBe` [4] 
            it "produces a product of 500 " $ 
                (multPairs [(100, 5)]) `shouldBe` [500]
            it "produces a product of [20, 42]" $
                (multPairs [(4, 5), (6, 7)]) `shouldBe` [20, 42]
        describe "squareList" $ do 
            it "produces the square of each value in the list [(1,1),(3,9),(2,4)]" $
                (squareList [1,3,2]) `shouldBe` [(1,1),(3,9),(2,4)]
            it "produces the square of each value in the list [(4,16),(5,25),(6,36)]" $
                (squareList [4, 5, 6]) `shouldBe` [(4,16),(5,25),(6,36)]
            it "produces the swuare of each value in the list [(17,289),(9,81),(10,100)]" $ 
                (squareList [17, 9, 10]) `shouldBe` [(17,289),(9,81),(10,100)]
        describe "findLowercase" $ do 
            it "produces T/F based on first letter in the string being lowercase or not" $ 
                (findLowercase ["hElLoO"]) `shouldBe` [True]
            it "" $
                (findLowercase ["HElLoO", "world"]) `shouldBe` [False, True]
            it "" $ 
                (findLowercase ["my name is", "Sunjoi", "gandhi"]) `shouldBe` [True,False,True]

