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
  prop "number" $ \i ->
    parseExpr (show (i :: Integer)) == Right (Number i)

  prop "string" $ \s ->
    parseExpr (show (s :: String)) === Right (Str s)

  specify "variable" $
    parseExpr "asdf" `shouldBe` Right (Var "asdf")

  specify "boolean" $ do
    parseExpr "True" `shouldBe` Right (Boolean True)
    parseExpr "False" `shouldBe` Right (Boolean False)

  specify "function" $
    parseExpr "foo a = a" `shouldBe` Right (Func "foo" "a" (Var "a"))

  specify "function application" $ do
    parseExpr "foo a" `shouldBe` Right (App (Var "foo") (Var "a"))
    parseExpr "foo a b" `shouldBe` Right (App (App (Var "foo") (Var "a")) (Var "b"))

  specify "expr" $
    parseExpr "a + b * c / d" `shouldBe` Right (BinOp "+" (Var "a") (BinOp "/" (BinOp "*" (Var "b") (Var "c")) (Var "d")))
