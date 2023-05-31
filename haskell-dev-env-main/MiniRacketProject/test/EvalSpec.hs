module EvalSpec where


import Test.Hspec
import Parser
import Expr
import MiniRacketParser

import Eval
import Error

type ParseResult = Either ErrorT (Expr, String)

spec :: Spec
spec = do
    describe "eval expressions" $ do
        it "evaluates number: 1235" $ 
            evalStr "1235" `shouldBe` Right (IntVal 1235)
        it "evaluates negative numbers: -12235" $
            evalStr "-12235" `shouldBe` Right (IntVal (-12235))

        it "evaluates true" $
            evalStr "true" `shouldBe` Right (BoolVal True)
        it "evaluates false" $
            evalStr "false" `shouldBe` Right (BoolVal False)
    
        it "evaluates and" $ 
            evalStr "(and true (and false true) true)" `shouldBe` Right (BoolVal False)
        it "evaluares or" $ 
            evalStr "(or true(and true false) false)" `shouldBe` Right (BoolVal True)

        it "evaluates not" $ 
            evalStr "(not true)" `shouldBe` Right (BoolVal False)
        it "evaluates not" $ 
            evalStr "(not false)" `shouldBe` Right (BoolVal True)

        it "evaluates Eq" $ 
            evalStr "(equals? 5 5)" `shouldBe` Right (BoolVal True)
        it "evaluates Lt" $ 
            evalStr "(< 6 7)" `shouldBe` Right (BoolVal True)

        it "evaluates math" $
            evalStr "(+ 5 5)" `shouldBe` Right (IntVal 10)
        it "evaluates math" $ 
            evalStr "(- 7 4)" `shouldBe` Right (IntVal 3)
        it "evaluates math" $ 
            evalStr "(* 9 0)" `shouldBe` Right (IntVal 0)
        it "evaluates math" $ 
            evalStr "(div 10 10)" `shouldBe` Right (IntVal 1)
        it "evaluates math" $ 
            evalStr "(mod 5 2)" `shouldBe`Right (IntVal 1) 