module WeeklyHaskellOneSpec where

import Test.Hspec ( hspec, describe, it, shouldBe, Spec )
import WeeklyHaskellOne

main::IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "removeChar" $ do
        it "produces a new string 'ello" $
            removeChar 'h' "hello" `shouldBe` "ello"
        it "produces a new string Sunjo" $
            removeChar 'i' "Sunjoi" `shouldBe` "Sunjo"
    describe "removeWhitespace" $ do
        it "produces a new string 'helloworld'" $
            removeWhitespace "hello world" `shouldBe` "helloworld"
        it "produces a new string SunjoiGandhi" $
            removeWhitespace "Sunjoi Gandhi" `shouldBe` "SunjoiGandhi"
    describe "removePunctuation" $ do
        it "produces a new string 'hello" $
            removePunctuation "(hello)" `shouldBe` "hello"
        it "produces a new string YAY" $
            removePunctuation "[Y[A]Y]" `shouldBe` "YAY"
    describe "charsToAscii" $ do
        it "produces the ascii value of chars [97]" $
            charsToAscii "a" `shouldBe` [97]
        it "produces the ascii value of chars [104,105]" $
            charsToAscii "hi" `shouldBe` [104, 105]
    describe "asciiToChars" $ do
        it "produces the character 'a'" $
            asciiToChars [97] `shouldBe` "a"
        it "produces the string 'hi'" $
            asciiToChars [104, 105] `shouldBe` "hi"
    describe "shiftInts" $ do
        it "produces a new list of integers" $
            shiftInts 1 [2, 4, 6, 127] `shouldBe` [3, 5, 7, 0]
        it "produces a new list of intgers" $
            shiftInts 2 [8, 10, 125] `shouldBe` [10, 12, 127]
    describe "shiftMessage" $ do
        it "encodes a message out of a string" $
            shiftMessage 5 "hello" `shouldBe` "mjqqt"
        it "encodes a message out of a string" $
            shiftMessage 20 "Sunjoi" `shouldBe` "g\t\STX~\ETX}"