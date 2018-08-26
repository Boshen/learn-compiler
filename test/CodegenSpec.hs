module CodegenSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Codegen
import           Syntax

spec :: Spec
spec = describe "Codegen" $ do
  prop "Number" $ \i ->
    gen (Number (i :: Integer)) == show i

  prop "Var" $ \s -> not (null s) ==>
    gen (Var (s :: String)) == s

  prop "BinOp" $ forAll (elements ["+", "-", "*", "/"]) $ \op ->
    gen (BinOp op (Var "a") (Var "b")) == "a" ++ op ++ "b"
