backspace :: String -> String
backspace [] = []
backspace [x] | x == '#' = []
                | otherwise = [x]
backspace (x:y:xs) | y == '#' = backspace xs
                   | otherwise = x:backspace(y:xs)

cleanString :: String -> String
cleanString [] = []
cleanString xs | elem '#' xs = cleanString (backspace xs)
               | otherwise = xs
