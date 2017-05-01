{-# OPTIONS_GHC -Wall#-}

module TypeCheck where

import Data.Map (Map)
import qualified Data.Map as Map
import Syntax

type Context = Map VarName Type


data TypeError =  ExpectedFunction Exp Type
                | ExpectedTuple Exp Type
                | Mismatch Exp Type Type {- expression, got, expected -}
                | UnboundVariable VarName deriving Show


check :: Exp -> Either TypeError Type
check e = typeOf Map.empty e

typeOf :: Context -> Exp -> Either TypeError Type
typeOf g (Var x) =
  case Map.lookup x g of
    Nothing -> Left $ UnboundVariable x
    Just t -> pure t
typeOf _ (Bool _) = pure TBool
typeOf _ (Int _) = pure TInt
typeOf g (Lambda x t1 e) = do
  t2 <- typeOf (Map.insert x t1 g) e
  pure $ TFun t1 t2
typeOf g (Tuple e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  pure $ TTuple t1 t2
typeOf g e@(Apply e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  case t1 of
    TFun t11 t12 | t11 == t2 -> pure t12
    TFun t11 _ -> Left $ Mismatch e t2 t11
    _ -> Left $ ExpectedFunction e1 t1
typeOf g e@(Unop Neg e') = do
  t <- typeOf g e'
  case t of
    TInt -> pure TInt
    t' -> Left $ Mismatch e t' t
typeOf g e@(Unop Not e') = do
  t <- typeOf g e'
  case t of
    TBool -> pure TBool
    t' -> Left $ Mismatch e t' t
typeOf g (Unop Fst (Tuple e1 _)) = typeOf g e1
typeOf g (Unop Snd (Tuple _ e2)) = typeOf g e2
typeOf g e@(Unop _ e') = do
  t <- typeOf g e'
  Left $ ExpectedTuple e t



--typeOf g (If e1 e2 e3) = do
--  t1 <- typeOf g e1
--  t2 <- typeOf g e2
--  t3 <- typeOf g e3
--  case t1 of
--    TBool | t2 == t3 -> pure t2
--    TBool -> Left $ Mismatch e3 {- arbitrary! -} t3 t2
--    _ -> Left $ Mismatch e1 t1 TBool

