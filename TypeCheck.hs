{-# OPTIONS_GHC -Wall#-}

module TypeCheck where

import Data.Map (Map)
import qualified Data.Map as Map
import Syntax

type Context = Map VarName Type


data TypeError =  ExpectedFunction Exp Type
                | ExpectedTuple Exp Type
                | Mismatch Exp Type Type {- expression, got, expected -}
                | BinopMismatch Exp Type Type Type {- expression, got 1, got 2, expected -}
                | UnboundVariable VarName deriving Show


checkType :: Exp -> Either TypeError Type
checkType e = typeOf Map.empty e

typeOf :: Context -> Exp -> Either TypeError Type
typeOf g (Var x) =
  case Map.lookup x g of
    Nothing -> Left $ UnboundVariable x
    Just t -> return t
typeOf _ (Bool _) = return TBool
typeOf _ (Int _) = return TInt
typeOf g (Type e t) = do
  t' <- typeOf g e
  if t == t' then return t else Left $ Mismatch e t' t
typeOf g (Lambda x t1 e) = do
  t2 <- typeOf (Map.insert x t1 g) e
  return $ TFun t1 t2
typeOf g (Tuple e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  return $ TTuple t1 t2
typeOf g e@(Apply e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  case t1 of
    TFun t11 t12 | t11 == t2 -> return t12
    TFun t11 _ -> Left $ Mismatch e t2 t11
    _ -> Left $ ExpectedFunction e1 t1
typeOf g e@(Unop Neg e') = do
  t <- typeOf g e'
  case t of
    TInt -> return TInt
    t' -> Left $ Mismatch e t' t
typeOf g e@(Unop Not e') = do
  t <- typeOf g e'
  case t of
    TBool -> return TBool
    t' -> Left $ Mismatch e t' t
typeOf g e@(Unop Fst e') = do --
  t <- typeOf g e'
  case t of
    TTuple t1 _ -> return t1
    _ -> Left $ ExpectedTuple e t
typeOf g e@(Unop Snd e') = do --
  t <- typeOf g e'
  case t of
    TTuple _ t2 -> return t2
    _ -> Left $ ExpectedTuple e t
typeOf g (If e1 e2 e3) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  t3 <- typeOf g e3
  case t1 of
    TBool | t2 == t3 -> return t2
    TBool -> Left $ Mismatch e3 {- arbitrary! -} t3 t2
    _ -> Left $ Mismatch e1 t1 TBool
typeOf g (Binop Equal e1 e2) = do 
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  if t1 == t2 then return TBool else Left $ Mismatch e2 t2 t1
typeOf g e@(Binop b e1 e2) | b `elem` numOps = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  if t1 == TInt && t1 == t2
  then if b == Lt then return TBool else return TInt
  else Left $ BinopMismatch e t1 t2 TInt
typeOf g e@(Binop _ e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  if t1 == TBool && t1 == t2 then return TBool 
  else Left $ BinopMismatch e t1 t2 TBool
typeOf g (Let v e1 e2) = do
  t1 <- typeOf g e1
  typeOf (Map.insert v t1 g) e2
typeOf g (LetRec v t e1 e2) = do
  t1 <- typeOf (Map.insert v t g) e1
  if t == t1 then 
    case t1 of
      (TFun _ _) -> typeOf (Map.insert v t g) e2
      _ -> Left $ ExpectedFunction e1 t1
  else Left $ Mismatch e1 t1 t

numOps = [Times, Div, Plus, Sub, Lt]
boolOps = [And, Or]