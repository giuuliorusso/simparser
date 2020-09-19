{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module SimParser
  ( module SimParser,
    module Control.Applicative,
  )
where

import Control.Applicative
import Data.Char
import Text.Printf

--

newtype ParserError = ParserError String deriving (Show)

instance Alternative (Either ParserError) where
  empty = Left $ ParserError "Empty!"
  Left _ <|> x = x
  x <|> _ = x

--

newtype Parser a = Parser {parse :: String -> Either ParserError (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, xs) <- p input
    return (f x, xs)

instance Applicative Parser where
  pure v = Parser $ \input -> Right (v, input)

  (Parser pf) <*> (Parser px) = Parser $ \input -> do
    (f, out) <- pf input
    (x, out') <- px out
    return (f x, out')

instance Monad Parser where
  p >>= f = Parser $ \input -> do
    (x, xs) <- parse p input
    (x, xs) <- parse (f x) xs
    return (x, xs)

instance Alternative Parser where
  empty = Parser $ \_ -> Left $ ParserError "Parsing failed!" -- TODO

  (Parser pf) <|> (Parser pg) = Parser $ \input -> pf input <|> pg input

--

item :: Parser Char
item = Parser $ \case
  "" -> Left $ ParserError "Parsing failed: end of string reached"
  (x : xs) -> Right (x, xs)

sat :: (Char -> Bool) -> String -> Parser Char
sat p desc = do
  x <- item
  if p x
    then return x
    else Parser $ \_ -> Left $ ParserError $ printf "Parsing failed: expected %s, found '%c'" desc x

--

digit :: Parser Char
digit = sat isDigit "digit"

char :: Char -> Parser Char
char c = sat (== c) $ printf "'%c'" c

lower :: Parser Char
lower = sat isLower "lowercase char"

upper :: Parser Char
upper = sat isUpper "uppercase char"

letter :: Parser Char
letter = sat isAlpha "letter"

alphanum :: Parser Char
alphanum = sat isAlphaNum "alphanumeric char"

string :: String -> Parser String
string "" = return ""
string s@(x : xs) = do
  char x
  string xs
  return s

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
nat = read <$> some digit

int :: Parser Int
int =
  do
    char '-'
    negate <$> nat
    <|> nat

--

space :: Parser ()
space = do
  many $ sat isSpace "space"
  return ()

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol = token . string
