module MiniRacketParserSpec where 

import Test.Hspec
import Parser
import Expr 
import MiniRacketParser
import Error ( ErrorT(NoParse, SyntaxError, ParseError) )

type ParseResult = Either ErrorT (Expr, String)

main :: IO() 
main = hspec spec

expr :: Either ErrorT (a2, b) -> a2
expr (Right (e, _)) = e 
expr (Left (SyntaxError msg)) = error msg
expr (Left (ParseError msg)) = error msg
expr (Left NoParse) = error "no matching parse"
expr _ = error "expr in MiniRacketParser.hs is not fully implemented yet..."

spec :: Spec
spec = do
    describe "parse literals" $ do 
        it "parses number: 1235" $
            parseStr "1235" `shouldBe` Right (LiteralExpr (IntVal 1235),"")
        it "parses negative numbers: -12235" $
            parseStr "-12235" `shouldBe` Right (LiteralExpr (IntVal (-12235)),"")
        it "parses true" $
            parseStr "true" `shouldBe` Right (LiteralExpr (BoolVal True),"")
        it "parses false" $
            parseStr "false" `shouldBe` Right (LiteralExpr (BoolVal False),"") 
    describe "parse boolean operations" $ do 
        it "parses and" $ 
            parse parseBoolOp "and" `shouldBe` Right (And,"")
        it "parses or" $ 
            parse parseBoolOp "or" `shouldBe` Right (Or,"")
    describe "parse math operations" $ do 
        it "parses add" $ 
            parse parseMathOp "+" `shouldBe` Right (Add,"")
        it "parses subtract" $ 
            parse parseMathOp "-" `shouldBe` Right (Sub,"")
        it "parses multiplication" $ 
            parse parseMathOp "*" `shouldBe` Right (Mul,"")
        it "parses divides" $ 
            parse parseMathOp "div" `shouldBe` Right (Div,"")
        it "parses modulus" $ 
            parse parseMathOp "mod" `shouldBe` Right (Mod,"")
    describe "parse compare operations" $ do
        it "parses equals?" $ 
            parse parseCompOp "equals?" `shouldBe` Right (Eq,"")
        it "parses <" $ 
            parse parseCompOp "<" `shouldBe` Right (Lt,"")
    describe "parse not" $ do 
        it "parses not correctly" $
            parse notExpr "not 1" `shouldBe` Right (NotExpr (LiteralExpr (IntVal 1)),"")
        it "parses not correctly" $ 
            parse notExpr "not" `shouldBe` Left NoParse
        it "parses not correctly" $
            parse notExpr "not 5" `shouldBe` Right (NotExpr (LiteralExpr (IntVal 5)),"")
   

