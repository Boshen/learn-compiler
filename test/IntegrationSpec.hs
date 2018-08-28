module IntegrationSpec where

import           Test.Hspec

import           Codegen
import           Parser

spec :: Spec
spec = describe "Integration" $ do
  specify "test 1" $ do
    let
      source = "\\ x y -> let x = if x then x else y in x + 2"
      target = "(x)=>(y)=>const x=()=>{if(x)(then)(x)(else)(y);return x(2)}"

    case parseExpr source of
      Right expr -> gen expr `shouldBe` target
      Left err   -> expectationFailure (show err)
