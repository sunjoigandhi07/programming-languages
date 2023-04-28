module DailySevenSpec where 
    import Test.Hspec 
    import DailySeven

    main::IO () 
    main = hspec spec   

    spec :: Spec   
    spec = do 
        describe "findLongest" $ do 
            it "produces a string " $ 
                (findLongest []) `shouldBe` [] 
            it "produces a string" $
                findLongest ["Hi", "Hello"] `shouldBe` "Hello"
            it "produces a string" $
                findLongest ["My", "name", "is", "Sunjoi"] `shouldBe` "Sunjoi"
        describe "anyLarger" $ do 
            it "produces a bool 'True'" $
                anyLarger 10 [11, 12, 9] `shouldBe` True
            it "produces a bool 'True'" $
                anyLarger 5 [1, 3, 5, 2] `shouldBe` True
            it "produces a bool 'False'" $ 
                anyLarger 4 [1, 2, 3] `shouldBe` False  
        describe "allNames" $ do  
            it "produces a string" $ 
                allNames [("kermit", "the frog"), ("bugs", "bunny")] `shouldBe` "kermit the frog, bugs bunny"
            it "produces a string" $
                allNames[] `shouldBe` ""
            it "produces a string" $ 
                allNames [("Sunjoi", "Gandhi")] `shouldBe` "Sunjoi Gandhi"