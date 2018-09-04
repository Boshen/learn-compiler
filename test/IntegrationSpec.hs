module IntegrationSpec where

import           Control.Monad
import           Data.Void
import           Test.Hspec
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Codegen
import           Parser

spec :: Spec
spec =
  beforeAll (readFile "./test/cases/case1.hs") $
  describe "Integration" $
  specify "test" $ \file ->
    case parse (many $ count 2 testCase) "" file of
      Left err -> expectationFailure (show err)
      Right files ->
        forM_ files $ \[source, target] ->
          case parseExpr source of
            Right expr -> gen expr `shouldBe` target
            Left err   -> expectationFailure (show err)

testCase :: Parsec Void String String
testCase = do
  void $ string "----"
  void space1
  void $ many (alphaNumChar <|> char '.')
  void newline
  code <- manyTill (asciiChar <|> newline) (string "\n\n")
  return code
