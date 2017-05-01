{-# OPTIONS_GHC -Wall#-}

module Syntax where

import Control.Applicative
import Data.Char
--import Data.Map (Map)
--import qualified Data.Map as Map

type VarName = String

data Type = TInt | TBool | TFun Type Type | TTuple Type Type deriving (Eq, Show)

data Exp = 
    Var VarName
  | Apply Exp Exp
  | Lambda VarName Type Exp
  | Let VarName Exp Exp
  | LetRec VarName Type Exp Exp
  | Bool Bool
  | Type Exp Type
  | Int Int
  | Tuple Exp Exp
  | If Exp Exp Exp
  | Unop Unop Exp
  deriving (Eq, Show)


data Unop = Neg | Not | Fst | Snd
            deriving (Eq, Show)


--instance Show Exp where
--  show = showExp

-- TODO: improve show
showExp :: Exp -> String
showExp (Var v) = v
showExp (Apply (Var v1) (Var v2)) = v1 ++ " " ++ v2
showExp (Apply (Var v) e2) = v ++ " (" ++ show e2 ++ ")"
showExp (Apply e1 (Var v)) = "(" ++ show e1 ++ ") " ++ v
showExp (Apply e1 e2) = show e1 ++ " (" ++ show e2 ++ ")"
--showExp (Lambda v t (Lambda v' t' e)) = "lambda " ++ v ++ " " ++ v' ++ ". " ++ show e
showExp (Lambda v t e) = "lambda " ++ v ++ ":" ++ show t ++ ". " ++ show e
showExp (Let v e1 e2) = "let " ++ v ++ " = " ++ show e1 ++ " in \n" ++ show e2
showExp (Bool b) = if b then "true" else "false"

newtype Parser a = Parser { parse :: String -> Maybe (a,String) }


instance Functor Parser where
  fmap f p = Parser $ \s -> (\(a,c) -> (f a, c)) <$> parse p s

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a,s)
  f <*> a = Parser $ \s ->
    case parse f s of
      Just (g,s') -> parse (fmap g a) s'
      Nothing -> Nothing

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  l <|> r = Parser $ \s -> parse l s <|> parse r s

-- ensures the next element parsed satisfy some requirement
ensure :: (a -> Bool) -> Parser a -> Parser a
ensure p parser = Parser $ \s ->
   case parse parser s of
     Nothing -> Nothing
     Just (a,s') -> if p a then Just (a,s') else Nothing

ensure2 :: (a -> Bool) -> (a -> Bool) -> Parser a -> Parser a
ensure2 p p' parser = Parser $ \s ->
   case parse parser s of
     Nothing -> Nothing
     Just (a,s') -> if p a && p' a then Just (a,s') else Nothing

-- parse one character
lookahead :: Parser (Maybe Char)
lookahead = Parser f
  where f [] = Just (Nothing,[])
        f (c:s) = Just (Just c,c:s)

-- parse a character only if it satisfy some requirement
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f [] = Nothing
        f (x:xs) = if p x then Just (x,xs) else Nothing

-- parse the eof
eof :: Parser ()
eof = Parser $ \s -> if null s then Just ((),[]) else Nothing

-- parse white spaces
ws :: Parser ()
ws = pure () <* many (satisfy isSpace)

-- parse a certain character
char :: Char -> Parser Char
char c = ws *> satisfy (==c)

-- parse a certain string
str :: String -> Parser String
str s = ws *> loop s
  where loop [] = pure []
        loop (c:cs) = (:) <$> satisfy (==c) <*> loop cs

-- parse the parenthesis
parens :: Parser a -> Parser a
parens p = (char '(' *> p) <* char ')'

keywords :: [String]
keywords = ["let", "in", "lambda", "true", "false", "if", "else", "then", "rec", "and",
            "or", "not", "fst", "snd"]

notKeyword :: String -> Bool
notKeyword = not . (`elem` keywords)

numOf' :: String -> Bool
numOf' s = (2::Int) > foldr (\x b -> if x == '\'' then (b+1) else b) 0 s

isAlphaNum' :: Char -> Bool
isAlphaNum' c = isAlphaNum c || c == '\''

-- parse a variable
var :: Parser String
var = ws *> ensure2 numOf' notKeyword ((:) <$> 
            satisfy isAlpha <*> many (satisfy isAlphaNum'))

int :: Parser Int
int = read <$> some (satisfy isDigit)

bool :: Parser Bool
bool =      pure True <* str "true" <* ws
        <|> pure False <* str "false" <* ws 
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p sep = foldl (\acc (op,v) -> op acc v) <$> 
                p <*> many ((\op v -> (op,v)) <$> sep <*> p)

lcSyntax :: Parser Exp
lcSyntax =     Unop <$> pure Neg <* char '-'  <*> lcExp
           <|> Unop <$> pure Not <* str "not" <*> lcExp
           <|> Unop <$> pure Fst <* str "fst" <*> lcExp
           <|> Unop <$> pure Snd <* str "snd" <*> lcExp
           <|> (lcExp `chainl1` (pure Apply))

lcType :: Parser Type
lcType =     TFun <$> lcType' <* str "->" <*> lcType
         <|> TTuple <$> (char '(' *> lcType) <* char ',' <*> lcType <* char ')'
         <|> lcType'

lcType' :: Parser Type
lcType' =    pure TBool <* str "bool"
         <|> pure TInt <* str "int"


lcExp :: Parser Exp
lcExp =     parens lcSyntax
        <|> Let <$> (str "let" *> var) <* char '=' <*> lcSyntax <* str "in" <*> lcSyntax
        <|> Let <$> (str "let" *> var) <*> ((flip Type) <$> lcType <* char '=' <*> lcSyntax) 
                <* str "in" <*> lcSyntax
        <|> LetRec <$> (str "let" *> str "rec" *> var) <* char ':' <*> lcType <* char '=' 
                   <*> lcSyntax <* str "in" <*> lcSyntax
        <|> Lambda <$> (str "lambda" *> var) <* char ':' <*> lcType <* char '.' <*> lcSyntax
        <|> Lambda <$> (str "lambda" *> char '(' *> var) <* char ':' <*> lcType <* char ')' <*> lcLamda
        <|> Var <$> var
        <|> Bool <$> bool
        <|> Type <$> (char '(' *> lcSyntax) <* char ':' <*> lcType <* char ')'
        <|> Int <$> (ws *> int)
        <|> Tuple <$> (char '(' *> lcSyntax) <* char ',' <*> lcSyntax <* char ')'
        <|> If <$> (str "if" *> lcSyntax) <* str "then" <*> lcSyntax <* str "else" <*> lcSyntax 

lcLamda :: Parser Exp
lcLamda =    Lambda <$> (char '(' *> var) <* char ':' <*> lcType <* char ')' <*> lcLamda
         <|> char '.' *> lcSyntax

tryParse :: Parser Exp -> String -> Either String Exp
tryParse parser s = 
    case parse parser s of
      Nothing -> Left "Cannot parse the expression"
      Just (x, "") -> Right x
      Just (_, s') -> Left $ "Expected EOF, got:" ++ s'

tryParse' :: Parser Type -> String -> Either String Type
tryParse' parser s = 
    case parse parser s of
      Nothing -> Left "Cannot parse the expression"
      Just (x, "") -> Right x
      Just (_, s') -> Left $ "Expected EOF, got:" ++ s'


parseLC :: String -> Either String Exp
parseLC = tryParse lcSyntax
