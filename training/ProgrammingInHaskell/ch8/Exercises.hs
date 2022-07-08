import Prelude hiding (return)

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
