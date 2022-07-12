import Prelude hiding ((^), and, concat, replicate, (!!), elem, sum, take, drop, last)

--
(^) :: Int -> Int -> Int
(^) _ 0 = 1
(^) p q = p * (^) p (q - 1)
--

--
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n p = [p] ++ replicate (n - 1) p

(!!) :: [a] -> Int -> a
(!!) xs 0 = head xs
(!!) (x:xs) n = (!!) xs (n - 1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) | x == y = True
              | otherwise = elem x ys
--

--
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

halve :: [a] -> [([a], [a])]
halve xs = [(take n xs, drop n xs)]
            where n = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort x) (msort y)
            where x = fst ((halve xs)!!0)
                  y = snd ((halve xs)!!0)
--

--
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

take :: Int -> [a] -> [a]
take 0 _ = []
take n (x:xs) = x:take (n - 1) xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop n (x:xs) = drop (n - 1) xs

last :: [a] -> a
last [x] = x
last (x:xs) = last xs
--
