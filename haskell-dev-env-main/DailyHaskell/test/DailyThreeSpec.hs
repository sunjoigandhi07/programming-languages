module DailyThreeSpec where 
    import Test.Hspec
    import DailyThree

    main::IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "removeAllExcept" $ do    
            it "produces removeAllExcept [] " $
                removeAllExcept 1 [] `shouldBe` []
            it "produces removeAllExcept [1, 1]" $
                removeAllExcept 1 [1, 2, 1, 3, 4, 5] `shouldBe` [1, 1]
        describe "countOccurances" $ do
            it "produces the number of occurances " $
                countOccurances 1 [] `shouldBe` 0
            it "produces the number of occurances" $
                countOccurances 4 [1, 3, 4, 5, 6, 4, 8, 4, 9]`shouldBe` 3
        describe "substitute" $ do 
            it "produces a new list with substituted values " $ 
                substitute 1 2 [] `shouldBe` []
            it "produces a new list with substituted values" $
                substitute 5 6 [1, 3, 5, 5, 7] `shouldBe` [1, 3, 6, 6, 7]
        