toBin :: Int -> [Int]
toBin n | n == 0 = []
        | otherwise = n `mod` 2 : toBin (n `div` 2)

printOnes :: Int -> IO ()
printOnes n = case n of
                0 -> putStr ""
                _ ->
                    do
                        putStrLn (show n ++ " - " ++ show (sum (toBin n)))
                        printOnes (n - 1)
