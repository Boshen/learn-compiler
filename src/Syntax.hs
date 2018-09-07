module Syntax where

type Name = String

data Expr
  = Number Integer
  | Var Name
  | Str String
  | Boolean Bool
  | If Expr Expr Expr
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

data Type
  = TBool
  | TNat
  | TStr
  | TArrow
  deriving (Eq, Show)
