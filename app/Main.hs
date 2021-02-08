module Main where

import Events
import UI.Board
import Graphics.Gloss.Interface.IO.Game
import Sokoban.Data
import Parser.Sokoban

initialState = parseBoard "####\n\
                          \#@$.#\n\
                          \#####\n"

main :: IO ()
main =  do
  playIO
    (InWindow "Sokoban" (500, 500) (1, 1))
    backgroundColor
    10
    initialState
    drawGame
    handleInput
    step