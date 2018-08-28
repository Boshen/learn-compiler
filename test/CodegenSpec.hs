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

  prop "Str" $ \s ->
    gen (Str (s :: String)) == show s

  prop "Var" $ \s -> not (null s) ==>
    gen (Var (s :: String)) == s

  prop "Boolean" $ \b ->
    gen (Boolean (b :: Bool)) == if b then "true" else "false"

  specify "Lambda" $ do
    gen (Lambda "foo" (Var "a")) `shouldBe` "(foo)=>a"
    gen (Lambda "foo" (Lambda "bar" (Var "baz"))) `shouldBe` "(foo)=>(bar)=>baz"

  specify "If" $
    gen (If (Var "x") (Var "y") (Var "z")) `shouldBe` "x ? y : z"

  specify "Let" $
    gen (Let "foo" (Var "x") (Var "y")) `shouldBe` "const foo=()=>{x;return y}"

  specify "App" $
    gen (App (Var "foo") (Var "bar")) `shouldBe` "foo(bar)"

  prop "BinOp" $ forAll (elements ["+", "-", "*", "/"]) $ \op ->
    gen (BinOp op (Var "a") (Var "b")) == "a" ++ op ++ "b"
