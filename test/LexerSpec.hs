module LexerSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Text.Megaparsec       (parse)

import qualified Lexer                 as L

spec :: Spec
spec = describe "Lexer" $ do
  prop "integer" $ \i ->
    parse L.integer "" (show (i :: Integer)) === Right i

  prop "str" $ \s ->
    parse L.str "" (s :: String) === Right s

  prop "parens" $ \i ->
    parse (L.parens L.integer) "" ("(" ++ show (i :: Integer) ++ ")") == Right i

  specify "identifier" $
    parse L.identifier "" "asdf" `shouldBe` Right "asdf"
