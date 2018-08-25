module LexerSpec where

import qualified Data.List             as List
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Text.Parsec           (parse)

import qualified Lexer                 as L

spec :: Spec
spec = describe "Lexer" $ do
  prop "integer" $ \i ->
    parse L.integer "" (show (i :: Integer)) == Right i

  prop "float" $ \i ->
    i >= 0 ==> parse L.float "" (show (i :: Double)) == Right i

  prop "parens" $ \i ->
    parse (L.parens L.integer) "" ("(" ++ show (i :: Integer) ++ ")") == Right i

  prop "commaSep" $ \list ->
    let s = List.intercalate "," (map show (list :: [Integer])) in
    parse (L.commaSep L.integer) "" s == Right list

  prop "semiSep" $ \list ->
    let s = List.intercalate ";" (map show (list :: [Integer])) in
    parse (L.semiSep L.integer) "" s == Right list

  specify "identifier" $
    parse L.identifier "" "asdf" `shouldBe` Right "asdf"

  specify "reserved" $ do
    parse (L.reserved "def") "" "def" `shouldBe` Right ()
    parse (L.reserved "extern") "" "extern" `shouldBe` Right ()
    parse (L.reserved "def") "" "asdf" `shouldSatisfy` (\(Left _) -> True)

  specify "reservedOp" $ do
    parse (L.reserved "+") "" "+" `shouldBe` Right ()
    parse (L.reserved "+") "" "-" `shouldSatisfy` (\(Left _) -> True)
