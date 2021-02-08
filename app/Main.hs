module Main where

import Graphics.Gloss.Interface.IO.Game
import Game.UI
import Game.Transitions
import Game.States
import Parser.Sokoban

loadedBoard = parseBoard "####\n\
                   \#@$.#\n\
                   \#####\n"

initialState = Game{ playingState=Play loadedBoard
                   , remainingBoards=[loadedBoard, loadedBoard]
                   , finishedAllLevels=False
                   }

draw :: Game -> IO Picture
draw = return . drawGame

input :: Event -> Game -> IO Game
input e = return . handleInput e

step :: Float -> Game -> IO Game
step dt = return . timeStep dt

background :: Color
background = black

main :: IO ()
main =  do
  playIO
    (InWindow "Sokoban" (500, 500) (1, 1))
    background
    10
    initialState
    draw
    input
    step