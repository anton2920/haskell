import Main2 hiding (eval, return)
import System.IO

getCh :: IO Char
getCh  = do
            hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

interactiveStrlen :: IO ()
interactiveStrlen = do
                        putStr "Enter a string: "
                        xs <- getLine
                        putStr "The string has "
                        putStr (show (length xs))
                        putStrLn " characters"

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p str = do
                    goto p
                    putStr str

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do
                a
                seqn as

--- Calculator
box :: [String]
box = [ "┌───────────────┐",
        "|               |",
        "├───┬───┬───┬───┤",
        "| q | c | d | = |",
        "├───┼───┼───┼───┤",
        "| 1 | 2 | 3 | + |",
        "├───┼───┼───┼───┤",
        "| 4 | 5 | 6 | ─ |",
        "├───┼───┼───┼───┤",
        "| 7 | 8 | 9 | * |",
        "├───┼───┼───┼───┤",
        "| 0 | ( | ) | / |",
        "└───┴───┴───┴───┘"]

buttons :: [Char]
buttons = standard ++ extra
            where
                standard = "qcd=123+456-789*0()/"
                extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [writeat (1, y) xs | (y, xs) <- zip [1..13] box]

display :: String -> IO ()
display xs = do
                writeat (3, 2) "               "
                writeat (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do
            display xs
            c <- getCh
            if elem c buttons then
                process c xs
            else
                do
                    beep
                    calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC"      = quit
             | elem c "dD\BS\DEL"   = delete xs
             | elem c "=\n"         = eval xs
             | elem c "cC"          = clear
             | otherwise            = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
            [(n, "")] -> calc (show n)
            [(_, inv)] -> calc ("error near: " ++ inv)
            _ -> do beep
                    calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

runCalc :: IO ()
runCalc = do
            cls
            showbox
            clear

--- Game of life

width :: Int
width = 5

height :: Int
height = 5

type Board = [Pos]

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

showcells :: Board -> IO ()
showcells b = seqn [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [ (x - 1, y - 1), (x, y - 1),
                            (x + 1, y - 1), (x - 1, y),
                            (x + 1, y), (x - 1, y + 1),
                            (x, y + 1), (x + 1, y + 1)]

wrap :: Pos -> Pos
wrap (x, y) = ( ((x - 1) `mod` width) + 1,
                ((y - 1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

{-births :: Board -> [Pos]
births b = [(x, y) |    x <- [1..width],
                        y <- [1..height],
                        isEmpty b (x, y),
                        liveneighbs b (x, y) == 3]-}

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do
            cls
            showcells b
            wait 100000
            life (nextgen b)

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]

--- Exercises
---
readLine :: IO String
readLine =  do
                x <- getCh
                putStr (if x == '\DEL' then "\ESC[1D \ESC[1D" else [x])
                if x == '\n' then
                    return []
                else
                    do
                        xs <- readLine
                        return (if null xs || head xs /= '\DEL' then (x:xs) else tail xs)
---
