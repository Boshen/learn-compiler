module LexerSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Test.Hspec.QuickCheck
import           Text.Megaparsec

import qualified Lexer                 as L

prs :: Parsec e s a -> s -> Either (ParseError (Token s) e) a
prs p = parse p ""

spec :: Spec
spec = describe "Lexer" $ do
  specify "space consumer" $ do
    prs L.sc "--" `shouldParse` ()
    prs L.sc "{-- --}" `shouldParse` ()

  prop "integer" $ \i ->
    prs L.integer (show (i :: Integer)) `shouldParse` i

  prop "str" $ \s ->
    prs L.str (show s :: String) `shouldParse` s

  prop "symbol" $ \s ->
    prs (L.symbol (s :: String)) s `shouldParse` s

  prop "parens" $ \i ->
    prs (L.parens L.integer) ("(" ++ show (i :: Integer) ++ ")") `shouldParse` i

  specify "identifier" $ do
    prs L.identifier "asdf" `shouldParse` "asdf"
    prs L.identifier `shouldFailOn` "let"
