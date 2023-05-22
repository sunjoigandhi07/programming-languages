module DailyTenSpec where 
    import Test.Hspec
    import DailyTen
    import Data.Char 

    main::IO () 
    main = hspec spec   

    spec :: Spec   
    spec = do 
        describe "firstFunctorLaw" $ do
            it "produces a T/F based on the contents of the input " $
                firstFunctorLaw (Just ('c', 35)) `shouldBe` True
            it "produces a T/F based on the contents of the input" $
                firstFunctorLaw [2, 3, 5, 7, 11] `shouldBe`True
            it "produces T/F for Right Nothing" $
                firstFunctorLaw (Right Nothing :: Either String (Maybe Integer)) `shouldBe` True
            it "produces T/F for Right (Just k)" $
                firstFunctorLaw (Right (Just 24) :: Either String (Maybe Integer)) `shouldBe` True
            it "produces T/F for Left value" $
                firstFunctorLaw (Left "error message" :: Either String (Maybe Integer)) `shouldBe` True
        describe "secondFunctorLaw" $ do 
            it "produces a T/F based on the contents of the input" $
                secondFunctorLaw isAlpha fst (Just ('c', 35)) `shouldBe` True
            it "produces T/F based on the contents of the input" $ 
                secondFunctorLaw chr (+96) [2, 3, 5, 7, 11] `shouldBe` True 
            it "produces T/F for Right Nothing" $
                secondFunctorLaw (fmap (+15)) (fmap(*3))  (Right Nothing :: Either String (Maybe Integer)) `shouldBe` True
            it "produces T/F for Right (Just k)" $
                secondFunctorLaw (fmap (+15)) (fmap(*3)) (Right (Just 42) :: Either String (Maybe Integer)) `shouldBe` True
            it "produces T/F for Left value" $
                secondFunctorLaw (fmap (+15)) (fmap(*3)) (Left "error message" :: Either String (Maybe Integer)) `shouldBe` True

