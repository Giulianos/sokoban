module Main where

import Graphics.Gloss.Interface.IO.Game
import Game.UI
import Game.Transitions
import Game.States
import Sokoban.Data
import Parser.Sokoban

buildGameState :: [Board] -> Game
buildGameState boards = let (first:remaining) = boards
                        in Game{ playingState=Play first
                               , remainingBoards=remaining
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
  fileContent <- readFile "boards.txt"
  playIO
    (InWindow "Sokoban" (500, 500) (1, 1))
    background
    10
    (buildGameState (parseBoards fileContent))
    draw
    input
    step