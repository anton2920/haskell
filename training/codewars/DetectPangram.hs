import Data.Char
import Data.List

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lesser ++ equals ++ qsort greater
                where
                  lesser  = filter (< x) xs
                  equals  = filter (== x) (x:xs)
                  greater = filter (> x) xs

getLetters :: String -> String
getLetters = nub . qsort . filter (isAlpha) . map (toLower)

isPangram :: String -> Bool
isPangram xs = getLetters xs == ['a'..'z']
