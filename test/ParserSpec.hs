module ParserSpec where

import qualified Data.List             as List
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Text.Parsec           (parse)

import           Parser
import           Syntax

spec :: Spec
spec = describe "Parser" $ do
  prop "int" $ \i ->
    parseExpr (show (i :: Integer)) == Right (Float (fromInteger i))

  prop "floating" $ \i -> i >= 0 ==>
    parseExpr (show (i :: Double)) == Right (Float i)

  specify "variable" $
    parseExpr "asdf" `shouldBe` Right (Var "asdf")

  specify "function" $ do
    parseExpr "def foo() a" `shouldBe` Right (Function "foo" [] (Var "a"))
    parseExpr "def foo(a b) a" `shouldBe` Right (Function "foo" [Var "a", Var "b"] (Var "a"))

  specify "extern" $ do
    parseExpr "extern foo()" `shouldBe` Right (Extern "foo" [])
    parseExpr "extern foo(a b)" `shouldBe` Right (Extern "foo" [Var "a", Var "b"])

  specify "call" $ do
    parseExpr "foo()" `shouldBe` Right (Call "foo" [])
    parseExpr "foo(a, b)" `shouldBe` Right (Call "foo" [Var "a", Var "b"])

  specify "expr" $
    parseExpr "a + b * c / d" `shouldBe` Right (BinOp Plus (Var "a") (BinOp Divide (BinOp Times (Var "b") (Var "c")) (Var "d")))

  it "should parse valid programs" $
    let
      program = "def foo(x y) x+foo(y, 4.0)"
      ast = Function "foo" [Var "x",Var "y"] (BinOp Plus (Var "x") (Call "foo" [Var "y",Float 4.0]))
    in
      parseExpr program `shouldBe` Right ast
