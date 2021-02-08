module Parser.Sokoban where

import Parser.Monadic
import Sokoban.Data
import Prelude hiding (floor)

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
row = many (do cell)

board :: Parser Board
board = many (do r <- row; char '\n'; return r)

boards :: Parser [Board]
boards = board `sepBy` string "---"

parseBoards :: String -> [Board]
parseBoards s = fst (head (parse boards s))