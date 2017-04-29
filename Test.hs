
module Test where

--lcProg1, lcProg2, lcProg3, lcProg4, lcProg2', lcProg2'' :: String
--lcProg1 = "s z"
--lcProg2 = "lambda x . x"
--lcProg2' = "lambda x . x x"
--lcProg2'' = "lambda x . x x x"
--lcProg3 = "lambda x . y"
--lcProg4 = "lambda x . x lambda y . y"

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
