module Syntax where

type Name = String

data Expr
  = Number Integer
  | Var Name
  | BinOp String
          Expr
          Expr
  deriving (Eq, Ord, Show)
