module Parser
  ( parseExpr
  ) where

import           Text.Megaparsec
import qualified Text.Megaparsec.Expr as Ex

import           Lexer                (Parser)
import qualified Lexer                as L
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
  L.symbol "\\"
  args <- some L.identifier
  L.symbol "->"
  body <- expr
  return $ foldr Lambda body args

ifExpr :: Parser Expr
ifExpr = do
  L.symbol "if"
  e1 <- expr
  L.symbol "then"
  e2 <- expr
  L.symbol "else"
  e3 <- expr
  return $ If e1 e2 e3

letExpr :: Parser Expr
letExpr = do
  L.symbol "let"
  var <- L.identifier
  L.symbol "="
  ex <- expr
  L.symbol "in"
  body <- expr
  return $ Let var ex body

factor :: Parser Expr
factor = try $ choice [ L.parens expr, variable, str, number, lambda, ifExpr, letExpr ]

expr :: Parser Expr
expr = Ex.makeExprParser factor opTable

opTable :: [[Ex.Operator Parser Expr]]
opTable =
  [ [Ex.InfixL spacef ]
  , [binary "*" "*" , binary "/" "/" ]
  , [binary "+" "+" , binary "-" "-" ]
  ]
  where
    binary s f = Ex.InfixL (L.symbol s >> return (BinOp f))
    spacef =
      L.sc *> notFollowedBy (choice . map L.symbol $ L.opNames) >>
      return App

contents :: Parser a -> Parser a
contents p = do
  L.sc
  r <- p
  eof
  return r

parseExpr = parse (contents expr) "<stdin>"
