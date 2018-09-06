module TypeCheckSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Syntax
import           Type
import           TypeCheck

spec :: Spec
spec =
  describe "TypeCheck" $ do
    prop "Number" $ \i ->
      typeCheck (Number i) == Right TNat

    specify "Var"
      pending

    prop "Str" $ \s ->
      typeCheck (Str s) == Right TStr

    prop "Boolean" $ \b ->
      typeCheck (Boolean b) == Right TBool

    specify "If" $ do
      typeCheck (If (Boolean True) (Number 1) (Number 2)) `shouldBe` Right TNat
      typeCheck (If (Number 1) (Number 1) (Number 2)) `shouldBe` Left (TypeMismatch TNat TBool)
      typeCheck (If (Boolean True) (Number 1) (Boolean True)) `shouldBe` Left (TypeMismatch TNat TBool)

    specify "Lambda" $ do
      typeCheck (Lambda "foo" (Number 1)) `shouldBe` Right TNat

    specify "Let" $ do
      pending

    specify "App" $ do
      pending

    specify "BinOp" $ do
      typeCheck (BinOp "+" (Number 1) (Number 2)) `shouldBe` Right TNat
      typeCheck (BinOp "+" (Boolean True) (Number 2)) `shouldBe` Left (TypeMismatch TBool TNat)
