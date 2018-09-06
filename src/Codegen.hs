module Codegen where

import           Syntax

gen :: Expr -> String
gen (Number a) = show a

gen (Var a) = a

gen (Str s) = show s

gen (Boolean b) =
  if b
    then "true"
    else "false"

gen (Lambda arg e) = "(" ++ arg ++ ")" ++ " => " ++ gen e

gen (If e1 e2 e3) = gen e1 ++ " ? " ++ check e2 ++ " : " ++ check e3
  where
    check :: Expr -> String
    check e =
      case e of
        Var _ -> gen e
        _     -> "(" ++ gen e ++ ")"

gen (Let name e1 e2) =
  "const " ++ name ++ " = " ++ "() => { " ++ gen e1 ++ "; return " ++ gen e2 ++ " }"

gen (App e1 e2) = gen e1 ++ "(" ++ gen e2 ++ ")"

gen (BinOp op e1 e2) = gen e1 ++ " " ++ op ++ " " ++ gen e2
