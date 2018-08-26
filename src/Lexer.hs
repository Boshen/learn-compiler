module Lexer where

import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as T

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef

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

reserved :: String -> Parser ()
reserved = T.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer
