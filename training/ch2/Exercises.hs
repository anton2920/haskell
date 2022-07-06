-- Correcting mistakes
{-
N = a ’div’ length xs
    where
        a = 10
      xs = [1, 2, 3, 4, 5]
-}

{-
    1. Indentation
    2. ’ -> `
    3. N -> n
-}
n = a `div` length xs
    where
        a = 10
        xs = [1, 2, 3, 4, 5]

llast :: [a] -> a
llast xs = (drop ((length xs) - 1) xs)!!0

linit :: [a] -> [a]
linit xs = take((length xs) - 1) xs
