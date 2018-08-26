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

  specify "variable" $
    parseExpr "asdf" `shouldBe` Right (Var "asdf")

  specify "expr" $
    parseExpr "a + b * c / d" `shouldBe` Right (BinOp "+" (Var "a") (BinOp "/" (BinOp "*" (Var "b") (Var "c")) (Var "d")))
