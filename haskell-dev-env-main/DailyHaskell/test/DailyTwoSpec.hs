module DailyTwoSpec where

import Test.Hspec
import DailyTwo

main::IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "every4th" $ do
        it "produces every4th element [] " $
            every4th [1, 2, 3] `shouldBe` []
        it "produces every4th element [4,8]" $
            every4th [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] `shouldBe` [4,8]
    describe "tupleDotQuotient" $ do
        it "produces the tuple dot quotient 0.0 " $
            tupleDotQuotient [] [] `shouldBe` 0.0
        it "produces the tuple dot quotient [1, 2, 3] [4, 5, 6]" $
            tupleDotQuotient [1, 2, 3] [4, 5, 6]`shouldBe` 1.15
    describe "appendToEach" $ do 
        it "produces an edited list of Strings" $ 
            appendToEach "YAY!" ["YES", "SLAY"] `shouldBe` ["YESYAY!", "SLAYYAY!"]
        it "produces an edited list of Strings" $
            appendToEach "!!!!" ["My", "Name", "is", "Sunjoi"] `shouldBe` ["My!!!!", "Name!!!!", "is!!!!", "Sunjoi!!!!"]
    describe "toSetList" $ do 
        it "produces a list of integers with no duplicates" $
            toSetList[] `shouldBe` [] 
        it "produces a list of integers with no duplicates" $ 
            toSetList [1, 3, 5, 5, 7, 7, 9] `shouldBe` [1, 3, 5, 7, 9]
