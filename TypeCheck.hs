{-# OPTIONS_GHC -Wall#-}

module TypeCheck where

import Data.Map (Map)
import qualified Data.Map as Map
import Syntax

type Context = Map VarName Type

check :: Exp -> Either TypeError Type
check e = typeOf Map.empty e

typeOf :: Context -> Exp -> Either TypeError Type
typeOf g (Var x) =
  case Map.lookup x g of
    Nothing -> Left $ UnboundVariable x
    Just t -> pure t
typeOf g (Lambda x t1 e) = do
  t2 <- typeOf (Map.insert x t1 g) e
  pure $ TFun t1 t2
typeOf g e@(Apply e1 e2) = do
  t1 <- typeOf g e1
  t2 <- typeOf g e2
  case t1 of
    TFun t11 t12 | t11 == t2 -> pure t12
    TFun t11 _ -> Left $ Mismatch e t2 t11
    _ -> Left $ ExpectedFunction e1 t1
typeOf _ (Bool _) = pure TBool
--typeOf g (If e1 e2 e3) = do
--  t1 <- typeOf g e1
--  t2 <- typeOf g e2
--  t3 <- typeOf g e3
--  case t1 of
--    TBool | t2 == t3 -> pure t2
--    TBool -> Left $ Mismatch e3 {- arbitrary! -} t3 t2
--    _ -> Left $ Mismatch e1 t1 TBool
