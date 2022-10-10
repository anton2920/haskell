import           Data.Char

--
squares100 = sum [x^2 | x <- [1..n]]
                where n = 100
--

--
lreplicate :: Int -> a -> [a]
lreplicate 0 _ = []
lreplicate n p = p:lreplicate (n - 1) p
--

--
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]
--

--
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [6..n], x == (sum (init (factors x)))]
--

--
task51 = [(x, y) | x <- [1..3], y <- [4..6]]

task52 = concat [[(x, y) | y <- [4..6]] | x <- [1..3]]
--

--
find :: Eq a => a -> [(a, b)] -> [b]
find k v = [x | (k', x) <- v, k' == k]

positions :: Eq a => a -> [a] -> [Int]
positions v xs = find v (zip xs [0..length xs - 1])
--

--
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
--

--
let2int :: Char -> Int
let2int c = ord c - ord (if isLower c then 'a' else 'A')

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

int2ulet :: Int -> Char
int2ulet n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = int2ulet((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

frTable = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: String -> Int
lowers xs = length (filter isLower xs)

uppers :: String -> Int
uppers xs = length (filter isUpper xs)

count :: Char -> String -> Int
count c xs = length (filter (==c) xs)

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
            where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
            where
                factor = head (positions (minimum chitab) chitab)
                chitab = [chisqr (rotate n table') frTable | n <- [0..length frTable - 1]]
                table' = freqs xs
--
