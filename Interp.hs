{-# OPTIONS_GHC -Wall#-}

module Interp where

import Syntax
import TypeCheck

interp :: Exp -> Either String Exp
interp (Var v) = return $ Var v
interp (Apply e1 e2) = do
    e1' <- interp e1
    e2' <- interp e2
    case e1' of
        Lambda v _ e -> interp (bSub v e2' e)
        e -> Left $ "Applied non-function." ++ (show e)
interp (Let v e1 e2) = interp (bSub v e1 e2)
interp (LetRec v t e1 e2) = interp (bSub v (LetRec v t e1 (Var v)) (bSub v e1 e2))
interp (Lambda v t e) = return $ Lambda v t e
interp (Bool b) = return $ Bool b
interp (Int n) = return $ Int n
interp (Tuple e1 e2) = do
    e1' <- interp e1
    e2' <- interp e2
    return $ Tuple e1' e2'
interp (If e1 e2 e3) = do
    e1' <- interp e1
    case e1' of
        Bool True -> interp e2
        Bool False -> interp e3 
        _ -> Left "Expect bool in if statement"
interp (Type e _) = interp e
interp (Unop Neg e) = do
    e' <- interp e
    case e' of
        Int b -> return $ Int (negate b)
        _ -> Left "Cannot negate non-integer"
interp (Unop Not e) = do
    e' <- interp e
    case e' of
        Bool b -> return $ Bool (not b)
        _ -> Left "Cannot 'not' non-boolean"
interp (Unop Fst e) = do
    e' <- interp e
    case e' of
        Tuple e1 _ -> interp e1 -- technically just e1, but just to be safe
        _ -> Left "Cannot call fst on non-tuple"
interp (Unop Snd e) = do
    e' <- interp e
    case e' of
        Tuple _ e2 -> interp e2 -- technically just e1, but just to be safe
        _ -> Left "Cannot call snd on non-tuple"
interp (Binop Equal e1 e2) = do
    e1' <- interp e1
    e2' <- interp e2
    return $ Bool (e1' == e2')
interp (Binop binop e1 e2) | binop `elem` boolOps = do
    e1' <- interp e1
    e2' <- interp e2
    case (e1', e2') of
        (Bool b1, Bool b2) -> return $ Bool $ (boolOpLookup binop) b1 b2
        _ -> Left $ "Cannot operate " ++ (show binop) ++ "on non-booleans"
interp (Binop binop e1 e2) = do -- must take two ints
    e1' <- interp e1
    e2' <- interp e2
    case (e1', e2') of
        (Int a, Int b) -> if binop == Lt then return $ Bool (a < b)
                              else return $ Int $ (numOpLookup binop) a b
        _ -> Left $ "Cannot operate " ++ (show binop) ++ "on non-ints"


boolOpLookup :: Binop -> (Bool -> Bool -> Bool)
boolOpLookup And = (&&)
boolOpLookup Or = (||)
boolOpLookup _ = undefined

numOpLookup :: Binop -> (Int -> Int -> Int)
numOpLookup Times = (*)
numOpLookup Div = div
numOpLookup Plus = (+)
numOpLookup Sub = (-)
numOpLookup _ = undefined

bSub :: VarName -> Exp -> Exp -> Exp
bSub v e (Var v') = if v' == v then e else Var v'
bSub v e (Apply e1 e2) = Apply (bSub v e e1) (bSub v e e2)
bSub v e (Let v' e1 e2) = if v' == v then Let v' e1 e2
                          else Let v' (bSub v e e1) (bSub v e e2)
bSub v e (Lambda v' t e') = if v' == v then Lambda v' t e'
                            else Lambda v' t (bSub v e e')
bSub v e e'@(LetRec v' t e1 e2) = if v' == v then e'
                                  else LetRec v' t (bSub v e e1) (bSub v e e2)
bSub _ _ (Bool b) = Bool b
bSub _ _ (Int n) = Int n
bSub v e (Type e' t) = Type (bSub v e e') t
bSub v e (Tuple e1 e2) = Tuple (bSub v e e1) (bSub v e e2)
bSub v e (If e1 e2 e3) = If (bSub v e e1) (bSub v e e2) (bSub v e e3)
bSub v e (Unop o e') = Unop o (bSub v e e')
bSub v e (Binop o e1 e2) = Binop o (bSub v e e1) (bSub v e e2)