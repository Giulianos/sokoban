{-# LANGUAGE LambdaCase #-}
module LevelParser ( parse
                   , cell
                   , row
                   , board
) where

import Control.Monad
import Data.Char(isSpace, isAlpha, isAlphaNum, isDigit, ord)
import Data
import Prelude hiding (floor)

-- Parser definition
newtype Parser a = Parser (String -> [(a,String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f  = Parser (\cs -> concat [parse (f a) cs' |
                                (a,cs') <- parse p cs])

instance Functor Parser where
  fmap f p = do f <$> p
  
instance Applicative Parser where
  pure = return
  p <*> q = do
      f <- p
      f <$> q

-- Parsers definitions
next :: Parser Char
next = Parser (\case
                [] -> []
                (c:cs') -> [(c, cs')]
              )

-- | a parser that always fails
empty :: Parser a
empty = Parser (const [])

-- | combine results of two parsers 
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> parse p cs ++
                         parse q cs)

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser (take 1 . parse (p+++q))

-- | parse a char satisfying a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do c <- next
               if p c then return c else empty 

-- | parse a specific char
char :: Char -> Parser Char
char c = satisfy (==c)

-- | parse a specific string
string :: String -> Parser String
string "" = return ""
string (c:cs) 
    = do char c; string cs; return (c:cs)

-- | repeat a parser zero or more times
many :: Parser a -> Parser [a]
many p = many1 p <|> return []

-- | repeat a parser one or more times
many1 :: Parser a -> Parser [a]
many1 p = do a<-p; as<-many p; return (a:as) 

-- | parse many whitespace charaters
spaces :: Parser String
spaces = many (satisfy isSpace)

-- | parse a token using a parser `p`
-- discarding any initial spaces
token :: Parser a -> Parser a
token p = spaces >> p

-- | parse a string token
symb :: String -> Parser String
symb s = token (string s)

-- Cells parsers
wall :: Parser Cell
wall = do char '#'; return Wall

player :: Parser Cell
player = do char '@'; return (Floor (Just Player))

playerOnStorage :: Parser Cell
playerOnStorage = do char '+'; return (Storage (Just Player))

box :: Parser Cell
box = do char '$'; return (Floor (Just Box))

boxOnStorage :: Parser Cell
boxOnStorage = do char '*'; return (Storage (Just Box))

storage :: Parser Cell
storage = do char '.'; return (Storage Nothing)

floor :: Parser Cell
floor = do char ' '; return (Floor Nothing)

cell :: Parser Cell
cell = wall <|> player <|> playerOnStorage <|> box <|> boxOnStorage <|> storage <|> floor

row :: Parser Row
row = many (do c <- cell; return c)

board :: Parser Board
board = many (do r <- row; char '\n'; return r)