module Lexer where

import           Text.Parsec.Language (haskellStyle)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as T

opNames :: [String]
opNames = []

reservedNames :: [String]
reservedNames = ["let", "in", "=", "\\", "->"]

lexer :: T.TokenParser ()
lexer =
  T.makeTokenParser
    haskellStyle {T.reservedOpNames = opNames, T.reservedNames = reservedNames}

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

integer :: Parser Integer
integer = T.integer lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = T.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = T.semiSep lexer

identifier :: Parser String
identifier = T.identifier lexer

str :: Parser String
str = T.stringLiteral lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer
