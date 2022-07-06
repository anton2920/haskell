lhalve :: [a] -> ([a], [a])
lhalve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

safetail :: [a] -> [a]
safetail [] = []
safetail xs = tail xs

or :: Bool -> Bool -> Bool
b `or` False = b
_ `or` True = True


{-
and :: Bool -> Bool -> Bool
and a b = if (a == True) then (if (b == True) then True else False) else False
-}

and :: Bool -> Bool -> Bool
and a b = if (a == False) then False else b

-- \x -> \y -> \z -> x * y * z
