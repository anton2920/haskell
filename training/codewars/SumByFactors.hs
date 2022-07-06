import Data.List

primeFactors :: Integer -> [Integer]
primeFactors = go (2:[3,5..]) where
    go (p:ps) n | p * p > n      = [n]
                | n `mod` p == 0 = p : go (p:ps) (n `div` p)
                | otherwise      = go ps n

umerge :: Ord a => [a] -> [a] -> [a]
umerge [] ys = ys
umerge xs [] = xs
umerge (x:xs) (y:ys) | x < y = x:umerge xs (y:ys)
                     | x == y = x:umerge xs ys
                     | otherwise = y:umerge (x:xs) ys

primeFactorsList :: [Integer]-> [Integer]
primeFactorsList [] = []
primeFactorsList (x:xs) = umerge (nub (primeFactors x)) (primeFactorsList xs)

sumOfDivided :: [Integer] -> [(Integer, Integer)]
sumOfDivided xs = [(x, y) | x <- primeFactorsList (map abs xs), y <- [sum (filter (\z -> (z `mod` x) == 0) xs)]]
