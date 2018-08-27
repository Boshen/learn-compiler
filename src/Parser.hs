module Parser
  ( parseExpr
  ) where

import           Data.Functor.Identity
import           Text.Parsec
import qualified Text.Parsec.Expr      as Ex
import           Text.Parsec.String    (Parser)
import qualified Text.Parsec.Token     as T

import qualified Lexer                 as L
import           Syntax

number :: Parser Expr
number = Number <$> L.integer

variable :: Parser Expr
variable = var <$> L.identifier
  where
    var "True"  = Boolean True
    var "False" = Boolean False
    var v       = Var v

str :: Parser Expr
str = Str <$> L.str

lambda :: Parser Expr
lambda = do
  name <- L.identifier
  args <- many1 L.identifier
  L.reserved "="
  body <- expr
  return $ foldr Lambda body args

factor :: Parser Expr
factor =
  try lambda <|> try variable <|> try number <|> try str <|> try (L.parens expr)

expr :: Parser Expr
expr = Ex.buildExpressionParser opTable factor

opTable :: [[Ex.Operator String () Identity Expr]]
opTable =
  [ [Ex.Infix spacef Ex.AssocLeft]
  , [binary "*" "*" Ex.AssocLeft, binary "/" "/" Ex.AssocLeft]
  , [binary "+" "+" Ex.AssocLeft, binary "-" "-" Ex.AssocLeft]
  ]
  where
    binary s f = Ex.Infix (L.reservedOp s >> return (BinOp f))
    spacef =
      L.whiteSpace *> notFollowedBy (choice . map L.reservedOp $ L.opNames) >>
      return App

contents :: Parser a -> Parser a
contents p = do
  T.whiteSpace L.lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"
