module DailyTwelveSpec where
    import Test.Hspec
    import DailyTwelve

    main::IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "onlyNothing" $ do
            it "produces a Bool if the function has been applied to the elements" $
                onlyNothing (const Nothing) [1, 2, 3] `shouldBe` True
            it "produces a Bool if the function has been applied to the elements" $
                onlyNothing (\x -> if x == 2 then Just "result" else Nothing) [1, 2, 3] `shouldBe` False
            it "produces a Bool if the function has been applied to the elements" $
                onlyNothing (const Nothing) [Nothing, Nothing, Nothing] `shouldBe` True
        describe "firstAnswer" $ do
            it "produces Just v or Nothing" $
                firstAnswer (\x -> if x == 2 then Just "result" else Nothing) [1, 2, 3] `shouldBe` Just "result"
            it "produces Just v or Nothing" $
                firstAnswer (\x -> if x > 10 then Just True else Nothing)  [5, 7, 15, 18] `shouldBe`  Just True
            it "produces Just v or Nothing" $
                firstAnswer (\x -> if even x then Just (x * 2) else Nothing) [1, 3, 5] `shouldBe` Nothing
        describe "allAnswers" $ do 
            it "produces Nothing or lstn" $
                allAnswers (\x -> if x `mod` 2 == 0 then Just [x, x * 2] else Nothing) [1, 2, 3, 4] `shouldBe` Nothing 
            it "produces Nothing or lstn" $
                allAnswers (\x -> Just [x]) [10, 20, 30] `shouldBe` Just [10, 20, 30]
            it "produces Nothing or lstn" $
                allAnswers (\x -> Just [x * 10, x * 20]) []`shouldBe` Just [] 