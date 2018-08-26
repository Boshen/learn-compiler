module Parser
  ( parseExpr
  ) where

import           Text.Parsec
import qualified Text.Parsec.Expr   as Ex
import           Text.Parsec.String (Parser)
import qualified Text.Parsec.Token  as T

import qualified Lexer              as L
import           Syntax

number :: Parser Expr
number = Number <$> L.integer

variable :: Parser Expr
variable = Var <$> L.identifier

factor :: Parser Expr
factor = try number <|> try variable <|> L.parens expr

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor
  where
    binary s f = Ex.Infix (L.reservedOp s >> return (BinOp f))
    table =
      [ [binary "*" "*" Ex.AssocLeft, binary "/" "/" Ex.AssocLeft]
      , [binary "+" "+" Ex.AssocLeft, binary "-" "-" Ex.AssocLeft]
      ]

contents :: Parser a -> Parser a
contents p = do
  T.whiteSpace L.lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"
