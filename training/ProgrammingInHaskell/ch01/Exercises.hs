-- Local definition of `sum`
lsum :: Num a => [a] -> a
lsum []     = 0
lsum (x:xs) = x + lsum xs

-- Local definition of `product`
lproduct :: Num a => [a] -> a
lproduct []     = 1
lproduct (x:xs) = x * lproduct xs

-- Local definition of three-way `qsort`
lqsort :: Ord a => [a] -> [a]
lqsort [] = []
lqsort (x:xs) = lqsort smaller ++ equal ++ lqsort larger
                where
                    equal = filter (==x) (x:xs)
                    smaller = filter (<x) xs
                    larger = filter (>x) xs

-- Local definition of dual-pivot five-way `qsort`
lqsort5 :: Ord a => [a] -> [a]
lqsort5 [] = []
lqsort5 [x] = [x]
lqsort5 (x:y:xs) = lqsort5 l1 ++ eq1 ++ lqsort5 g1l2 ++ eq2 ++ lqsort5 g2
                    where
                        l1 = filter (< min x y) xs
                        eq1 = filter (== min x y) (x:y:xs)
                        g1l2 = filter (\z -> (z > min x y) && (z < max x y)) xs
                        eq2 = filter (\z -> (z == max x y) && (x /= y)) (x:y:xs)
                        g2 = filter (> max x y) xs
