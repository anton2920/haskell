import Prelude hiding (return, (>>=), (+++))

import Data.Char

{-
    - Parser of type 'a'
    - Consumes input string and produces result
    - Returns list of results of type 'a' and corresponding unconsumed string
-}
type Parser a = String -> [(a, String)]

return :: a -> Parser a
return v = \input -> [(v, input)]

failure :: Parser a
failure = \input -> []

item :: Parser Char
item = \input -> case input of
                    [] -> []
                    (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse parser input = parser input

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \input -> case parse p input of
                        [] -> []
                        [(v, out)] -> parse (f v) out


getFirstThird :: Parser (Char, Char)
getFirstThird = item >>= \x ->
                item >>= \_ ->
                item >>= \y ->
                return (x, y)

{-getFirstThird :: Parser (Char, Char)
getFirstThird = do x <- item
                   item
                   y <- item
                   return (x, y)-}

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \input -> case parse p input of
                        [] -> parse q input
                        [(v, out)] -> [(v, out)]

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x ->
            if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

alphanum :: Parser Char
alphanum = sat isAlphaNum

lower :: Parser Char
lower = sat isLower

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = char x >>= \_ -> string xs >>= \_ -> return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = p >>= \v -> many p >>= \vs -> return (v:vs)

ident :: Parser String
ident = lower >>= \x -> many alphanum >>= \xs -> return (x:xs)

nat :: Parser Int
nat = many1 digit >>= \xs -> return (read xs)

space :: Parser ()
space = many (sat isSpace) >>= \_ -> return ()

---

token :: Parser a -> Parser a
token p = space >>= \_ -> p >>= \v -> space >>= \_ -> return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

listOfNatural :: Parser [Int]
listOfNatural = symbol "[" >>= \_ ->
                natural >>= \n ->
                many (symbol "," >>= \_ -> natural) >>= \ns ->
                symbol "]" >>= \_ ->
                return (n:ns)

---

{-
Grammar:
    expr   ::= term   (+ expr | e)
    term   ::= factor (* term | e)
    factor ::= (expr) | nat
    nat    ::= 0 | 1 | 2 | ...
-}

expr :: Parser Int
expr = term >>= \t -> (symbol "+" >>= \_ -> expr >>= \e -> return (t + e)) +++ return t

term :: Parser Int
term = factor >>= \f -> (symbol "*" >>= \_ -> term >>= \t -> return (f * t)) +++ return f

factor :: Parser Int
factor = (symbol "(" >>= \_ -> expr >>= \e -> symbol ")" >>= \_ -> return e) +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
            [(n, [])] -> n
            [(_, out)] -> error ("unused input " ++ out)
            [] -> error "invalid input"
