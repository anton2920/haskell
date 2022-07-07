isPrime :: Integer -> Bool
isPrime n | n == 1 = False
          | n == 2 = True
          | n `mod` 2 == 0 = False
          | otherwise = null [x | x <- [3, 5..floor(sqrt (fromIntegral n))], n `mod` x == 0]

properFractions :: Integer -> Integer
properFractions n | isPrime n = n - 1
                  | otherwise = toInteger (length [x | x <- [1..n-1], (gcd x n) == 1])
