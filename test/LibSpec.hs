module LibSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = describe "test" $
  prop "test" $ \i ->
    (i :: String) `shouldBe` i
