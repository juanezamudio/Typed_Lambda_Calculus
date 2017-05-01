{-# OPTIONS_GHC -Wall#-}

module Interp where

import Syntax

interp :: Exp -> Exp
interp (Var v) = Var v
interp (Apply e1 e2) = 
  case interp e1 of
    Lambda v t e -> case interp e2 of 
                    Var e2' -> interp (bSub v (Var e2') e)
                    e2' -> interp (bSub v e2' e)
    _ -> undefined
interp (Let v e1 e2) = interp (bSub v e1 e2)
interp (Lambda v t e) = Lambda v t e

bSub :: VarName -> Exp -> Exp -> Exp
bSub v e (Var v') = if v' == v then e else Var v'
bSub v e (Apply e1 e2) = Apply (bSub v e e1) (bSub v e e2)
bSub v e (Let v' e1 e2) = if v' == v then Let v' e1 e2
                          else Let v' (bSub v e e1) (bSub v e e2)
bSub v e (Lambda v' t e') = if v' == v then Lambda v' t e'
                          else Lambda v' t (bSub v e e')