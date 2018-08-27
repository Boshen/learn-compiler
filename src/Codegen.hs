module Codegen where

import           Data.List (intercalate)

import           Syntax

gen :: Expr -> String
gen (Number x) = show x
gen (Var x) = x
gen (Str s) = show s
gen (Boolean b) =
  if b
    then "true"
    else "false"
gen (Lambda arg e) = "(" ++ arg ++ ")" ++ "=>" ++ gen e
gen (Let name e1 e2) = "const " ++ name ++ "=" ++ "()=>{" ++ gen e1 ++ ";return " ++ gen e2 ++ "}"
gen (App e1 e2) = gen e1 ++ "(" ++ gen e2 ++ ")"
gen (BinOp op e1 e2) = gen e1 ++ op ++ gen e2
