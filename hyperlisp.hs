import Numeric
import Data.Char
import Text.ParserCombinators.Parsec

--evaluatior
data Sexp = Cons Sexp Sexp | Snoc Sexp Sexp | Zero | One
    deriving (Show, Eq)

car :: Sexp -> Sexp
car (Cons x _) = x
car (Snoc x _) = x
car Zero = Zero
car One = Zero

cdr :: Sexp -> Sexp
cdr (Cons _ x) = x
cdr (Snoc _ x) = x
cdr Zero = Zero
cdr One = Zero

atom :: Sexp -> Bool
atom (Snoc _ _) = True
atom One = True
atom _ = False

molecule :: Sexp -> Bool
molecule (Cons _ _) = True
molecule Zero = True
molecule _ = False

str2sexp :: String -> Sexp
str2sexp (x:xs) = Snoc (char2sexp x) (str2sexp xs)
str2sexp _ = Zero
char2sexp :: Char -> Sexp
char2sexp c = f $ (showIntAtBase 2 intToDigit $ ord c) ""
  where
    f ('1':xs) = Snoc One $ f xs
    f ('0':xs) = Snoc Zero $ f xs
    f _ = Zero

eval :: Sexp -> Sexp
eval x
  | atom(x) = apply (car x) $ cdr x
  | otherwise = apply (car x) $ evlis $ cdr x 

evlis :: Sexp -> Sexp
evlis Zero = Zero
evlis x
  | atom(x) = Cons (car x) $ evlis $ cdr x 
  | otherwise = Cons (eval $ car x) $ evlis $ cdr x 

apply :: Sexp -> Sexp -> Sexp
apply Zero _ = Zero
apply One x = car x
apply f x
  | f == str2sexp "eq" = 
    if car x == car (cdr x) 
    then One
    else Zero
  | f == str2sexp "cond" = evcon x
  | f == str2sexp "atom" =
    if atom (car x)
    then One
    else Zero
  | f == str2sexp "null" =
    case car x of
    Zero -> One
    _ -> Zero
  | f == str2sexp "car" = car $ car x
  | f == str2sexp "cdr" = cdr $ car x
  | f == str2sexp "cons" = Cons (car x) $ car $ cdr x
  | f == str2sexp "snoc" = Snoc (car x) $ car $ cdr x
  | molecule(f) && car f == str2sexp "lambda" =
    eval $ subst x (car $ cdr f) $ car $ cdr $ cdr f

evcon :: Sexp -> Sexp
evcon Zero = Zero
evcon x
  | atom $ eval $ car $ car x = eval $ car $ cdr $ car x
  | molecule $ eval $ car $ car x = evcon (cdr x)

subst :: Sexp -> Sexp -> Sexp -> Sexp
subst _ Zero b = b
subst x p b
  | atom p = point x $ car p
  | atom b = Snoc (subst x (car p) $ car b) $ subst x (cdr p) $ cdr b
  | atom b = Cons (subst x (car p) $ car b) $ subst x (cdr p) $ cdr b

point :: Sexp -> Sexp -> Sexp
point _ Zero = Zero
point x q
  | atom q = x
  | cdr q == Zero = point (car x) $ car q
  | otherwise = point (cdr x) $ cdr q

-- parser
p_toplevel :: CharParser() Sexp
p_toplevel = p_form
p_form = 
  try(
  do char '('
     left <- p_form
     char '.'
     right <- p_form
     char ')'
     return $ Cons left right)
 <|> do char '('
        lst <- sepBy p_form spaces
        char ')'
        return $ foldr Cons Zero lst
 <|> 
 try(
     do char '['
        left <- p_form
        char '.'
        right <- p_form
        char ']'
        return $ Snoc left right)
 <|> do char '['
        lst <- sepBy p_form spaces
        char ']'
        return $ foldr Snoc Zero lst
 <|> (char '0' >> return Zero)
 <|> (char '1' >> return One)
 <|> p_literal
p_literal = do
  literal <- many1 p_char
  return $ str2sexp literal
p_char = oneOf ['a'..'z']

run str = case parse p_toplevel "hyperlisp" str of
  Right val -> Right $ eval val
  Left err ->  Left err
-- [(lambda.((0.([(1.0).0].([(1.0).0].0))).([cons.[0.1]].0))).[ab.0]]

