module Parser (parseExpr, parseToplevel) where

import           Text.Parsec
import qualified Text.Parsec.Expr   as Ex
import           Text.Parsec.String (Parser)
import qualified Text.Parsec.Token  as T

import qualified Lexer              as L
import           Syntax

int :: Parser Expr
int = Float . fromInteger <$> L.integer

floating :: Parser Expr
floating = Float <$> L.float

variable :: Parser Expr
variable = Var <$> L.identifier

function :: Parser Expr
function = do
  L.reserved "def"
  name <- L.identifier
  args <- L.parens $ many variable
  body <- expr
  return $ Function name args body

extern :: Parser Expr
extern = do
  L.reserved "extern"
  name <- L.identifier
  args <- L.parens $ many variable
  return $ Extern name args

call :: Parser Expr
call = do
  name <- L.identifier
  args <- L.parens $ L.commaSep expr
  return $ Call name args

factor :: Parser Expr
factor =
  try floating <|> try int <|> try extern <|> try function <|> try call <|>
  variable <|>
  L.parens expr

defn :: Parser Expr
defn = try extern <|> try function <|> expr

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor
  where
    binary s f = Ex.Infix (L.reservedOp s >> return (BinOp f))
    table =
      [ [binary "*" Times Ex.AssocLeft, binary "/" Divide Ex.AssocLeft]
      , [binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft]
      ]

contents :: Parser a -> Parser a
contents p = do
  T.whiteSpace L.lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel =
  many $ do
    def <- defn
    L.reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
