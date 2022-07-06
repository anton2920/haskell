-- Local definition of `sum`
lsum :: Num a => [a] -> a
lsum [] = 0
lsum (x:xs) = x + lsum xs

-- Local definition of `product`
lproduct :: Num a => [a] -> a
lproduct [] = 1
lproduct (x:xs) = x * lproduct xs

-- Local definition of three-way `qsort`
lqsort :: Ord a => [a] -> [a]
lqsort [] = []
lqsort (x:xs) = lqsort smaller ++ equal ++ lqsort larger
                where
                    equal = filter (==x) (x:xs)
                    smaller = filter (<x) xs
                    larger = filter (>x) xs
