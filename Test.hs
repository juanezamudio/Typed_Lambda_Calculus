
module Test where
import Syntax
import TypeCheck
import Interp

import Data.Map (Map)
import qualified Data.Map as Map

lcProg1 = "1" -- TInt
lcProg2 = "true" -- TBool
lcProg3 = "false"
lcProg4 = "lambda x:bool. x"
lcProg5 = "(lambda x:bool. x, 1)" -- TTuple
lcProg5' = "(1,1)" -- TTuple
lcProg6 = "(lambda x:bool. x) true" 
lcProg7 = "(lambda x:bool. x) 1" -- mismatch
lcProg8 = "(lambda x:bool->bool. x) (lambda x:bool.x)" -- function
lcProg9 = "(lambda x:bool->bool. x) (lambda x:bool->bool.x)" -- mismatch
lcProg10 = "(lambda x:bool->bool. x) 1" -- mismatch
lcProg11 = "if true then 1 else 2"
lcProg12 = "if (lambda x:bool.x) true then 1 else 2"
lcProg13 = "if true then 1 else true" -- should this program run?
lcProg14 = "not true"
lcProg15 = "if not false then 1 else 2"
lcProg16 = "if true then -1 else 1"
lcProg17 = "fst (1,2)"
lcProg18 = "snd (1,2)"
lcProg19 = "fst ((lambda x:(int, bool). x) (1,true))"
lcProg20 = "snd ((lambda x:(int, bool). x) (1,true))"
lcProg21 = "true or false"
lcProg22 = "true and false"
lcProg23 = "1==2"
lcProg24 = "1<=2"
lcProg25 = "1>=2"
lcProg25' = "1 >= 2"
lcProg26 = "1 < 2"
lcProg27 = "lambda x:int. x > 2"
lcProg28 = "(lambda x:int. x) 2 == 2"
lcProg29 = "1 + 1"
--lcProg26 = ""
--lcProg26 = ""
--lcProg26 = ""
--lcProg26 = ""
--lcProg26 = ""
--lcProg26 = ""
--lcProg26 = ""
--lcProg26 = ""
--lcProg26 = ""

testTypeChecker :: String -> Either String Type
testTypeChecker s = case parseLC s of 
                        Left e -> Left e
                        Right x -> case typeOf Map.empty x of 
                                    Left e' -> Left (show e')
                                    Right t -> Right t

testInterp :: String -> Either String Exp
testInterp s = do
    parsed <- parseLC s
    interp parsed

--t ::= int | bool | t1 -> t2 | (t1,t2)
--e ::= x | e1 e2 | lambda x : t. e
--    | if e1 then e2 else e3 | let x = e1 in e2 | let rec x : t = e1 in e2 | (e : t)
--    | n | true | false | (e1, e2)
--    | unop e | e1 binop e2
--unop ::= - | not | fst | snd
--binop ::= + | - | * | / | and | or | == | <

--zero = parseLC "lambda s z. z"
--succ = parseLC "lambda n. lambda s z. s (n s z)"
--two = parseLC "let zero = lambda s z. z in let succ = lambda n. lambda s z. s (n s z) in succ (succ zero)"

--pred = parseLC "\
--    \let zero = lambda s z. z in \
--    \let succ = lambda n. lambda s z. s (n s z) in \
--    \let pair = lambda a b. lambda c. c a b in \
--    \let fst = lambda p. p (lambda f t. f) in \
--    \let snd = lambda p. p (lambda f t. t) in \
--    \lambda n. snd (n (lambda p. pair (succ (fst p)) (fst p)) (pair zero zero))"
