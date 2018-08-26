module Codegen where

import Syntax

gen :: Expr -> String
gen (Number x) = show x
gen (Var x) = x
gen (BinOp op e1 e2) = gen e1 ++ op ++ gen e2
