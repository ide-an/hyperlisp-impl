import Numeric
import Data.Char
import Text.ParserCombinators.Parsec

data Sexp = Cons Sexp Sexp | Snoc Sexp Sexp | Zero
    deriving (Eq)
one = Snoc Zero Zero

instance Show Sexp where
  show (Snoc Zero Zero) = "1"
  show Zero = "0"
  show z@(Snoc x y) = "["++(showInner z False)++"]"
  show z@(Cons x y) = "("++(showInner z False)++")"

showInner :: Sexp -> Bool -> String
showInner Zero _ = ""
showInner (Snoc x y) t = (if t then "'" else "")++(show x)++(f y)
    where
      f y@(Cons _ _) = " "++(showInner y $ not t)
      f Zero         = ""
      f y            = " "++(showInner y t)
showInner (Cons x y) t = (if t then "'" else "")++(show x)++(f y)
    where
      f y@(Snoc _ _) = " "++(showInner y $ not t)
      f Zero         = ""
      f y            = " "++(showInner y t)

car :: Sexp -> Sexp
car (Cons x _) = x
car (Snoc x _) = x
car Zero = Zero

cdr :: Sexp -> Sexp
cdr (Cons _ x) = x
cdr (Snoc _ x) = x
cdr Zero = Zero

atom :: Sexp -> Bool
atom (Snoc _ _) = True
atom _ = False

molecule :: Sexp -> Bool
molecule (Cons _ _) = True
molecule Zero = True
molecule _ = False

--encode literal
str2sexp :: String -> Sexp
str2sexp (x:xs) = Snoc (char2sexp x) (str2sexp xs)
str2sexp _ = Zero
char2sexp :: Char -> Sexp
char2sexp c = f $ (showIntAtBase 2 intToDigit $ ord c) ""
  where
    f ('1':xs) = Snoc one $ f xs
    f ('0':xs) = Snoc Zero $ f xs
    f _ = Zero

--evaluator
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
apply f x
  | f == one = car x
  | f == str2sexp "eq" =
    if car x == car (cdr x)
    then one
    else Zero
  | f == str2sexp "cond" = evcon x
  | global_env f /= Zero = apply (global_env f) x
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
  | otherwise = Cons (subst x (car p) $ car b) $ subst x (cdr p) $ cdr b

point :: Sexp -> Sexp -> Sexp
point _ Zero = Zero
point x q
  | atom q = x
  | cdr q == Zero = point (car x) $ car q
  | otherwise = point (cdr x) $ cdr q

global_env :: Sexp -> Sexp
global_env x
  | x == str2sexp "car" = case parseForm "(lambda (0 [((1))]) [1 0])" of
    Right val -> val
    Left err -> Zero
  | x == str2sexp "cdr" = case parseForm "(lambda (0 [((0.1))]) [1 0])" of
    Right val -> val
    Left err -> Zero
  | x == str2sexp "null" = case parseForm "(lambda (0 [(1)]) [eq 0])" of
    Right val -> val
    Left err -> Zero
  | x == str2sexp "cons" = case parseForm "(lambda (0 ([(1)].[(0 1)])) [1])"of
    Right val -> val
    Left err -> Zero
  | x == str2sexp "snoc" = case parseForm "(lambda (0 ([(1)].[(0 1)])) [1 1])"of
    Right val -> val
    Left err -> Zero
  | x == str2sexp "atom" = case parseForm "(lambda (0 ((0 [(1)]))) [cond ([1 0] [1 1])])"of
    Right val -> val
    Left err -> Zero
  | otherwise = Zero

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
 <|> (char '1' >> return one)
 <|> p_literal
p_literal = do
  literal <- many1 p_char
  return $ str2sexp literal
p_char = oneOf ['a'..'z']

parseForm str = parse p_toplevel "hyperlisp" str
run str = case parseForm str of
  Right val -> Right $ eval val
  Left err ->  Left err
