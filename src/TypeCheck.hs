module TypeCheck
  ( typeCheck
  , TypeError(..)
  ) where

import           Control.Monad.Except

import           Syntax

data TypeError =
  TypeMismatch Type
               Type
  deriving (Eq, Show)

type Check a = Except TypeError a

typeCheck :: Expr -> Either TypeError Type
typeCheck = runExcept . typeof

typeof :: Expr -> Check Type
typeof ex = case ex of
  Number _ -> return TNat

  Var _ -> undefined

  Str _ -> return TStr

  Boolean _ -> return TBool

  If a b c -> do
    ta <- typeof a
    tb <- typeof b
    tc <- typeof c
    if ta /= TBool
    then throwError $ TypeMismatch ta TBool
    else
      if tb /= tc
      then throwError $ TypeMismatch tb tc
      else return tc

  Let{} -> undefined

  App{} -> undefined

  Lambda _ e -> typeof e

  BinOp _ a b -> do
    ta <- typeof a
    tb <- typeof b
    if ta /= tb
    then throwError $ TypeMismatch ta tb
    else return ta
