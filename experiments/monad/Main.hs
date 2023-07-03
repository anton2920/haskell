module Main where

newtype Parser a = Parser
	{ runParser :: String -> [(a, String)] }

instance Functor Parser where
	fmap :: (a -> b) -> Parser a -> Parser b
	fmap f pa = Parser (\xs -> case runParser pa xs of
					[]         -> []
					[(a, xs')] -> [(f a, xs')])

instance Applicative Parser where
	pure :: a -> Parser a
	pure x = Parser (\xs -> [(x, xs)])

	(<*>) :: Parser (a -> b) -> Parser a -> Parser b
	(<*>) f pa = Parser (\xs -> case runParser pa xs of
					[]         -> []
					[(a, xs')] -> case runParser f xs' of
							[]          -> []
							[(g, xs'')] -> [(g a, xs'')])

instance Monad Parser where
	(>>=) :: Parser a -> (a -> Parser b) -> Parser b
	(>>=) pa f = Parser (\xs -> case runParser pa xs of
					[]         -> []
					[(a, xs')] -> runParser (f a) xs')

main :: IO ()
main = putStrLn "Hello, world!"
