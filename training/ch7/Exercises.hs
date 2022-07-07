import Prelude hiding (all, any, takeWhile, dropWhile, map, filter, curry, uncurry)
import Data.Char

--
task11 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
task11 f p xs = [f x | x <- xs, p x]

task12 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
task12 f p xs = map f (filter p xs)
--

--
-- Naive approach
{-all :: (a -> Bool) -> [a] -> Bool
all p [] = True
all p (x:xs) | p x == False = False
             | otherwise = all p xs-}

-- Pro approach
all :: (a -> Bool) -> [a] -> Bool
all p xs = length xs == length (filter p xs)

any :: (a -> Bool) -> [a] -> Bool
any p xs = not (null (filter p xs))

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs) | p x == False = []
                   | otherwise = x:takeWhile p xs

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs) | p x == True = dropWhile p xs
                   | otherwise = (x:xs)
--

--
map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr ((++) . (\x -> if p x == True then [x] else [])) []
--

--
dec2Int :: [Int] -> Int
dec2Int = foldl (\n x -> (n * 10) + x) 0

strToInt :: String -> Int
strToInt = foldl (\n x -> (n * 10) + digitToInt x) 0

intToStr :: Int -> String
intToStr 0 = []
intToStr n = intToStr (n `div` 10) ++ [intToDigit (n `mod` 10)]
--

--
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

sumsqreven :: [Int] -> Int
sumsqreven = sum . map (^2) . filter even

{-
    - Invalid: types are incompatible - sum breaks it all
    - Actual types: [a] -> [a] -> [b] -> Int
    - But expected:  a  ->  a  ->  a  ->  a
sumsqreven = compose [sum, map (^2), filter even]
-}
--

--
sumPair :: (Int, Int) -> Int
sumPair p = fst p + snd p

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \x -> \y -> f (x, y)

uncurry :: (a0 -> b0 -> c0) -> ((a0, b0) -> c0)
uncurry f = \(x, y) -> f x y
--

--
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x:unfold p h t (t x)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

map2 :: (a -> b) -> [a] -> [b]
map2 f = unfold null (f . head) tail

-- Predicate is not important (?)
{-iterate :: (a -> a) -> a -> [a]
iterate f = unfold (null) id f-}
--
