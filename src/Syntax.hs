module Syntax where

type Name = String

data Expr
  = Number Integer
  | Var Name
  | Str String
  | Boolean Bool
  | Lambda Name
           Expr
  | Let Name
        Expr
        Expr
  | App Expr
        Expr
  | BinOp String
          Expr
          Expr
  deriving (Eq, Ord, Show)
