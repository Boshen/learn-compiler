module ParserSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Parser
import           Syntax

spec :: Spec
spec = describe "Parser" $ do
  context "expressions" $ do
    prop "number" $ \i ->
      parseExpr (show (i :: Integer)) == Right (Number i)

    prop "string" $ \s ->
      parseExpr (show (s :: String)) === Right (Str s)

    specify "variable" $
      parseExpr "asdf" `shouldBe` Right (Var "asdf")

    specify "boolean" $ do
      parseExpr "True" `shouldBe` Right (Boolean True)
      parseExpr "False" `shouldBe` Right (Boolean False)

    specify "if then else" $
      parseExpr "if x then y else z" `shouldBe` Right (If (Var "x") (Var "y") (Var "z"))

    specify "lambda" $ do
      parseExpr "\\a -> a" `shouldBe` Right (Lambda "a" (Var "a"))
      parseExpr "\\a b -> c" `shouldBe` Right (Lambda "a" (Lambda "b" (Var "c")))

    specify "let" $
      parseExpr "let x = 3 in x" `shouldBe` Right (Let "x" (Number 3) (Var "x"))

    specify "function application" $ do
      parseExpr "foo a" `shouldBe` Right (App (Var "foo") (Var "a"))
      parseExpr "foo a b" `shouldBe` Right (App (App (Var "foo") (Var "a")) (Var "b"))

    specify "expr" $
      parseExpr "a + b * c / d" `shouldBe` Right (BinOp "+" (Var "a") (BinOp "/" (BinOp "*" (Var "b") (Var "c")) (Var "d")))

  context "defs" $
    specify "top level def" $ do
      parseDef "foo x = 1" `shouldBe` Right (Let "foo" (Lambda "x" (Number 1)) (Var "foo"))
      parseDef "foo x y = x" `shouldBe` Right (Let "foo" (Lambda "x" (Lambda "y" (Var "x"))) (Var "foo"))
