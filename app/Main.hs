module Main where

import Graphics.Gloss.Interface.IO.Game
import Game.UI
import Game.Transitions
import Game.States
import Sokoban.Data
import Parser.Sokoban

buildGameState :: [Board] -> Game
buildGameState boards = Game{ playingState=Title 3
                            , remainingBoards=boards
                            , finishedAllLevels=False
                            }

draw :: Game -> IO Picture
draw = return . drawGame

input :: Event -> Game -> IO Game
input e = return . handleInput e

step :: Float -> Game -> IO Game
step dt = return . timeStep dt

background :: Color
background = makeColorI 0 41 79 255

main :: IO ()
main =  do
  fileContent <- readFile "boards.txt"
  playIO
    (InWindow "Sokoban" (500, 500) (1, 1))
    background
    10
    (buildGameState (parseBoards fileContent))
    draw
    input
    step