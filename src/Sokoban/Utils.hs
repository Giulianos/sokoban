module Sokoban.Utils where

import Sokoban.Data

boardCols :: Board -> Int
boardCols = maximum . map length

boardRows :: Board -> Int
boardRows = length

boardSize :: Board -> (Float, Float)
boardSize b = (fromIntegral (boardRows b), fromIntegral (boardCols b))