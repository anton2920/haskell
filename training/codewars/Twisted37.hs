import Data.List

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

toDigits :: Int -> [Int]
toDigits = reverse . unfold (== 0) (`mod` 10) (`div` 10)

toNumber :: [Int] -> Int
toNumber = foldl (\n x -> n * 10 + x) 0

swap37d :: Int -> Int
swap37d x | x == 3 = 7
          | x == 7 = 3
          | otherwise = x

swap37 :: Int -> Int
swap37 = toNumber . map (swap37d) . toDigits

sortTwisted37 :: [Int] -> [Int]
sortTwisted37 = map (swap37) . sort . map (swap37)
