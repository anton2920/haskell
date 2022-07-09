import Prelude hiding (return, (>>=), (+++))

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


{-getFirstThird :: Parser (Char, Char)
getFirstThird = item >>= \x ->
                item >>= \_ ->
                item >>= \y ->
                return (x, y)-}

getFirstThird :: Parser (Char, Char)
getFirstThird = do x <- item
                   item
                   y <- item
                   return (x, y)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \input -> case parse p input of
                        [] -> parse q input
                        [(v, out)] -> [(v, out)]

{-sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
            if p x then return x else failure-}


