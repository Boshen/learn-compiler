module Codegen where

import Syntax

gen :: Expr -> String
gen (Number x) = show x
gen (Var x) = x
gen (Str s) = show s
gen (Boolean b) = if b then "true" else "false"
gen (Func name arg e) =
  "const " ++ name ++ " = " ++ "(" ++ arg ++ ") => " ++ gen e
gen (App e1 e2) = gen e1 ++ "(" ++ gen e2 ++ ")"
gen (BinOp op e1 e2) = gen e1 ++ op ++ gen e2
