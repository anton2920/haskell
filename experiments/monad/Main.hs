module Main where

import Control.Applicative
import Data.Char

newtype Parser a = Parser
	{runParser :: String -> [(a, String)]}

instance Functor Parser where
	fmap :: (a -> b) -> Parser a -> Parser b
	fmap f (Parser pa) = Parser (\xs -> do
				(a, xs') <- pa xs
				[(f a, xs')])

instance Applicative Parser where
	pure :: a -> Parser a
	pure x = Parser (\xs -> [(x, xs)])

	(<*>) :: Parser (a -> b) -> Parser a -> Parser b
	(<*>) (Parser pf) (Parser pa) = Parser (\xs -> do
					(g, xs') <- pf xs
					(a, xs'') <- pa xs'
					[(g a, xs'')])

instance Monad Parser where
	(>>=) :: Parser a -> (a -> Parser b) -> Parser b
	(>>=) (Parser pa) f = Parser (\xs -> do
				(a, xs') <- pa xs
				runParser (f a) xs')

instance Alternative Parser where
	empty :: Parser a
	empty = Parser (\_ -> [])
	
	(<|>) :: Parser a -> Parser a -> Parser a
	(<|>) f g = Parser (\xs -> case runParser f xs of
					[]         -> runParser g xs
					[(a, xs')] -> [(a, xs')])

charP :: Char -> Parser Char
charP x = Parser (\(y:ys) -> (if x == y then [(y, ys)] else []))

stringP :: String -> Parser String
stringP xs = mapM charP xs

spaceP :: Parser Char
spaceP = charP ' ' <|> charP '\t'

main :: IO ()
main = putStrLn $ show $ runParser (stringP "hello") "hello, world!"
