module WeeklyThreeSpec where
    import Test.Hspec
    import WeeklyThree

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "show" $ do
            it "shows the correct string" $ 
                show (Vec [1.0, 3.0, 5.0, 7.0]) `shouldBe` "Vec [1.0,3.0,5.0,7.0]"
            it "shows the correct string" $
                show (Vec [0.0, 0.0, 0.0]) `shouldBe` "Vec [0.0,0.0,0.0]"
            it "shows the correct string" $ 
                show (Vec [9.9999999999]) `shouldBe` "Vec [9.9999999999]"
        describe "addition" $ do
            it "performs addition correctly" $ 
                Vec [1.0, 2.0, 3.0, 4.0] + Vec [2.0, 3.0, 4.0, 5.0] `shouldBe` Vec [3.0, 5.0, 7.0, 9.0]
            it "performs addition correctly" $
                Vec [5.0, 5.0, 5.0] + Vec [8.0, 8.9, 9.7] `shouldBe` Vec [13.0,13.9,14.7]
            it "performs addition correctly" $ 
                Vec [11.56, 56.11] + Vec [17.18, 18.17] `shouldBe` Vec [28.740000000000002,74.28]
        describe "subtraction" $ do 
            it "performs element-wise subtraction" $ 
                Vec [1.0, 2.0, 3.0, 4.0] - Vec [2.0, 3.0, 4.0, 5.0] `shouldBe` Vec [-1.0, -1.0, -1.0, -1.0]
            it "performs element-wise subtraction" $ 
                Vec [5.0] - Vec [5.0] `shouldBe` Vec [0.0]
            it "performs element-wise subtraction" $ 
                Vec [16.25] - Vec [17.25] `shouldBe` Vec [-1.0]
        describe "multiplication" $ do
            it "performs element-wise multiplication" $ 
                Vec [1.0, 2.0, 3.0, 4.0] * Vec [2.0, 3.0, 4.0, 5.0] `shouldBe` Vec [2.0, 6.0, 12.0, 20.0]
            it "performs element-wise multiplication" $ 
                Vec [2.0, 2.0] * Vec [6.0, 6.0] `shouldBe` Vec [12.0,12.0]
            it "performs element-wise multiplication" $ 
                Vec [4.0, 5.0, 6.0] * Vec [7.0, 8.0, 9.0] `shouldBe` Vec [28.0,40.0,54.0]
        describe "Eq" $ do 
            it "checks for equality" $ 
                Vec [1.0, 2.0, 3.0, 4.0] == Vec [1.0, 2.0, 3.0, 4.0] `shouldBe` True
            it "checks for equality" $ 
                Vec [0.0] == Vec [0.1] `shouldBe` False 
            it "checks for equality" $
                Vec [1] == Vec [2.0] `shouldBe` False
        describe "Ord" $ do
            it "compares vectors correctly" $  
                compare (Vec [1.0, 2.0, 3.0, 4.0]) (Vec [2.0, 3.0, 4.0, 5.0]) `shouldBe` LT
            it "compares vectors correctly" $
                compare (Vec [2.0, 3.0, 4.0, 5.0]) (Vec [1.0, 2.0, 3.0, 4.0]) `shouldBe` GT
            it "compares vectors correctly" $
                compare (Vec [1.0, 2.0, 3.0, 4.0]) (Vec [1.0, 2.0, 3.0, 4.0]) `shouldBe` EQ
        describe "Magnitude" $ do 
            it "calculates the magnitude" $ 
                magnitude (Vec [3.0, 4.0]) `shouldBe` 5.0
            it "calculates the magnitude" $
                magnitude (Vec [5.0, 5.0]) `shouldBe` 7.0710678118654755
            it "calculates the magnitude" $
                magnitude (Vec [17.0, 1.5]) `shouldBe` 17.066048165876012
        describe "Semigroup & Monoid " $ do 
            it "has an identity element for addition" $ do
                Vec [1.0, 2.0, 3.0, 4.0] <> mempty `shouldBe` Vec [1.0, 2.0, 3.0, 4.0]
            it "has an identity element for addition" $ do
                mempty <> Vec [1.0, 2.0, 3.0, 4.0] `shouldBe` Vec [1.0, 2.0, 3.0, 4.0]


