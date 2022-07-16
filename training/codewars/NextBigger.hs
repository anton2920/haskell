toDigits :: Int -> [Int]
toDigits n | n == 0 = []
           | otherwise = n `mod` 10 : toDigits (n `div` 10)

toNumber :: [Int] -> Int
toNumber = foldr (\x xs -> xs * 10 + x) 0

swapBigger :: [Int] -> [Int]
swapBigger [] = []
swapBigger [x] = [x]
swapBigger (x:y:xs) | x > y = y:x:xs
                    | otherwise = x:swapBigger (y:xs)

nextBigger :: Int -> Int
nextBigger n | n == nextBigger' n = -1
             | otherwise = nextBigger' n
                where
                    nextBigger' = toNumber . swapBigger . toDigits
