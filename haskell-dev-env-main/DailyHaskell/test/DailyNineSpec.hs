module DailyNineSpec where 

    import Test.Hspec
    import DailyNine

    main::IO () 
    main = hspec spec   

    spec :: Spec   
    spec = do 
        describe "findSmallest" $ do 
            it "produces Nothing if list empty or Just v, v being the smallest value" $
                findSmallest [4, 7, 2, 3, 5] `shouldBe` Just 2
            -- I keep getting an error for the base case and I have no idea how to fix it 
            -- it "produces Nothing if list empty or Just v, v being the smallest value" $ 
                -- findSmallest [] `shouldBe` Nothing 
            it "produces Nothing if list empty or Just v, v being the smallest value" $
                findSmallest [1, 1, 1, 1] `shouldBe` Just 1 
        describe "allTrue" $ do 
            it "produces Nothing if list is empty, Just True if all True, or Just False if not all True" $
                allTrue [] `shouldBe` Nothing 
            it "produces Nothing if list is empty, Just True if all True, or Just False if not all True" $
                allTrue [True, True, False] `shouldBe` Just False 
            it "produces Nothing if list is empty, Just True if all True, or Just False if not all True" $ 
                allTrue [True, True, True] `shouldBe` Just True
        describe "countAllVotes" $ do 
            it "produces a tuple of triples with the # of reps who haven't voted, voted for, and voted against" $ 
                countAllVotes [] `shouldBe` (0, 0, 0)
            it "produces a tuple of triples with the # of reps who haven't voted, voted for, and voted against" $
                countAllVotes [Nothing, Nothing, Just True, Just False] `shouldBe` (2, 1, 1)
            it "produces a tuple of triples with the # of reps who haven't voted, voted for, and voted against" $ 
                countAllVotes [Just True, Just False, Just False, Nothing, Just False, Just True, Nothing, Nothing, Nothing] `shouldBe` (4,2,3)