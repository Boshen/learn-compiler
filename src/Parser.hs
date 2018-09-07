module Parser
  ( parseExpr
  , parseDef
  ) where

import           Control.Monad
import           Data.Void
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
  void $ L.symbol "\\"
  args <- some L.identifier
  void $ L.symbol "->"
  body <- expr
  return $ foldr Lambda body args

ifExpr :: Parser Expr
ifExpr = do
  void $ L.symbol "if"
  e1 <- expr
  void $ L.symbol "then"
  e2 <- expr
  void $ L.symbol "else"
  e3 <- expr
  return $ If e1 e2 e3

letExpr :: Parser Expr
letExpr = do
  void $ L.symbol "let"
  var <- L.identifier
  void $ L.symbol "="
  ex <- expr
  void $ L.symbol "in"
  body <- expr
  return $ Let var ex body

factor :: Parser Expr
factor =
  try $ choice [L.parens expr, variable, str, number, lambda, ifExpr, letExpr]

expr :: Parser Expr
expr = Ex.makeExprParser factor opTable

opTable :: [[Ex.Operator Parser Expr]]
opTable =
  [ [Ex.InfixL spacef]
  , [binary "*" "*", binary "/" "/"]
  , [binary "+" "+", binary "-" "-"]
  ]
  where
    binary s f = Ex.InfixL (L.symbol s >> return (BinOp f))
    spacef =
      L.sc *> notFollowedBy (choice . map L.symbol $ L.opNames) >> return App

def :: Parser Expr
def = do
  name:args <- some L.identifier
  void $ L.symbol "="
  body <- expr
  return $ Let name (foldr Lambda body args) (Var name)

contents :: Parser a -> Parser a
contents p = do
  L.sc
  r <- p
  eof
  return r
parseExpr :: String -> Either (ParseError Char Void) Expr
parseExpr = parse (contents expr) "<stdin>"

parseDef :: String -> Either (ParseError Char Void) Expr
parseDef = parse (contents def) "<stdin>"
