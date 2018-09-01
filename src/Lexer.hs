module Lexer where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

opNames :: [String]
opNames = []

reservedNames :: [String]
reservedNames = ["let", "in", "=", "\\", "->", "if", "then", "else"]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{--" "--}")

int :: Parser Integer
int = lexeme L.decimal

integer :: Parser Integer
integer = L.signed sc int

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

str :: Parser String
str = char '"' >> manyTill L.charLiteral (char '"')

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reservedNames
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x
