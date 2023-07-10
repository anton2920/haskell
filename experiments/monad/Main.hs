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
	(<|>) (Parser f) (Parser g) = Parser (\xs -> case f xs of
							[] -> g xs
							ys -> ys)

charP :: Char -> Parser Char
charP x = Parser (\ys' -> case ys' of
				[]     -> []
				(y:ys) -> (if x == y then [(y, ys)] else []))

predP :: (Char -> Bool) -> Parser String
predP p = Parser(\xs -> case span p xs of
				([], _) -> []
				(a, b)  -> [(a, b)])

tokP :: String -> Parser String
tokP xs = do
		spaceP
		r <- mapM charP xs
		spaceP
		return r
	where spaceP = many (predP isSpace)

{-
Grammar:
	expr   = term   ( [+, -] term   )*
	term   = factor ( [*, /] factor )*
	factor = exp    ( ^ factor      )*
	exp    = ([+, -])* neg
	neg    = \( expr \) | num
	num    = [0-9]+([.][0-9]*)?|[.][0-9]+
-}

exprP :: Parser Double
exprP = (do
		x  <- termP
		xs <- some (do
				op <- (tokP "+" <|> tokP "-")
				x' <- termP
				return (if op == "+" then x' else negate x'))
		return (foldl (+) x xs)) <|> termP

termP :: Parser Double
termP = (do
		x <- factorP
		xs <- some (do
				op <- (tokP "*" <|> tokP "/")
				x' <- factorP
				return (if op == "*" then x' else recip x'))
		return (foldl (*) x xs)) <|> factorP

factorP :: Parser Double
factorP = (do
		x <- expP
		tokP "^"
		y <- factorP
		return (x ^ (round y))) <|> expP

expP :: Parser Double
expP = do
		signs <- many (do
				sign <- tokP "+" <|> tokP "-"
				return (if sign == "+" then id else negate))
		x <- negP
		return (foldr ($) x signs)

negP :: Parser Double
negP = (do
		tokP "("
		x <- exprP
		tokP ")"
		return x) <|> numP

numP :: Parser Double
numP = (do
		integ <- predP isDigit
		charP '.' <|> return '.'
		fract <- (predP isDigit <|> return "0")
		return (read (integ ++ '.':fract))) <|>
	(do
		charP '.'
		fract <- predP isDigit
		return (read ('0':'.':fract)))

eval :: String -> Double
eval s = case runParser exprP s of
		[(a, "")]        -> a
		[(_, rs)]        -> error ("cannot parse at or near '" ++ rs ++ "'")
		[]               -> error "invalid expression"

main :: IO ()
main = putStrLn $ show $ eval "(123.45*(678.90 / (-2.5+ 11.5)-(((80 -(19))) *33.25)) / 20) - (123.45*(678.90 / (-2.5+ 11.5)-(((80 -(19))) *33.25)) / 20) + (13 - 2)/ -(-11) "
