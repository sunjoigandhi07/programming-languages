module DailyElevenSpec where 
    import Test.Hspec 
    import DailyEleven 

    main::IO () 
    main = hspec spec   

    spec :: Spec   
    spec = do 
        describe "allLefts" $ do
            it "produces a list of any Left values" $ do
                allLefts [Left "error1", Right 10, Left "error2", Right 20, Left "error3"] `shouldBe` ["error1", "error2", "error3"]
            it "produces a list of any left values" $ do
                allLefts [Left "error1", Right 20, Right 30] `shouldBe` ["error1"]
            --it "produces an empty list for an empty input list" $ do
                --allLefts [] `shouldBe` []:: [Either String Int]
        describe "produceStringOrSum" $ do
            it "produces a String when either parameter is a String" $ do
                produceStringOrSum (Left "Error") (Right 10) `shouldBe` Left "Error"
            it "produces the sum of the two integer parameters" $ do
                produceStringOrSum (Right 20) (Right 30) `shouldBe` Right 50
            it "produces the first String parameter when both parameters are Strings" $ do
                produceStringOrSum (Left "Warning") (Left "Error") `shouldBe` Left "Warning"
        describe "sumListOfEither" $ do
            it "produces the sum of all the integers in the list" $ do
                sumListOfEither [Right 10, Right 20, Right 30] `shouldBe` Right 60
            it "produces the first String in the list" $ do
                sumListOfEither [Left "Error", Right 10, Right 20] `shouldBe` Left "Error"
            --it "produces Right 0 for an empty input list" $ do
                --sumListOfEither []`shouldBe` Right 0 :: [Either String Int]