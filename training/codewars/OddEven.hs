sortMyString :: String -> String
sortMyString xs = evens ++ " " ++ odds
                    where
                        evens = [xs!!n | n <- [0, 2..size]]
                        odds  = [xs!!n | n <- [1, 3..size]]
                        size  = length xs - 1
