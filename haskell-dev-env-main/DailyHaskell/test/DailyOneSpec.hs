module DailyOneSpec where

import Test.Hspec
import DailyOne

main::IO ()
main = hspec spec 

spec :: Spec
spec = do 
    describe "quadratic" $ do 
        it "produces the quadratic 0.0" $
            (quadratic 0 0 0 0) `shouldBe` 0.0
        it "produces the quadratic 57" $
            (quadratic 1 2 3 4) `shouldBe` 57

    describe "scaleVector" $ do 
        it "produces the scale vector (15.0, 20.0)" $
            (scaleVector 5 (3, 4)) `shouldBe` (15.0, 20.0)
        it "produces the scale vector (20.0, 90.0)" $ 
            (scaleVector 10 (2, 9)) `shouldBe` (20.0, 90.0)

    describe "tripleDistance" $ do
       it "produces the cartesian distance 0.0" $ 
           (tripleDistance (0, 0, 0) (0, 0, 0)) `shouldBe` 0.0 
       it "produces the cartesian distance 6.928203230275509" $
           (tripleDistance (7, 8, 9) (3, 4, 5)) `shouldBe` 6.928203230275509 