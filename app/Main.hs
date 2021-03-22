module Main where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Bitmap
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

draw :: [Picture] -> Game -> IO Picture
draw pics = return . drawGame pics

input :: Event -> Game -> IO Game
input e = return . handleInput e

step :: Float -> Game -> IO Game
step dt = return . timeStep dt

background :: Color
background = makeColorI 30 30 30 255

main :: IO ()
main =  do
  fileContent <- readFile "boards.txt"
  wallBmp <- loadBMP "img/Wall.bmp"
  playerBmp <- loadBMP "img/Player.bmp"
  boxBmp <- loadBMP "img/Box.bmp"
  storedBoxBmp <- loadBMP "img/StoredBox.bmp"
  floorBmp <- loadBMP "img/Floor.bmp"
  storageBmp <- loadBMP "img/Storage.bmp"
  playIO
    (InWindow "Sokoban" (500, 500) (1, 1))
    background
    10
    (buildGameState (parseBoards fileContent))
    (draw [wallBmp, playerBmp, boxBmp, storedBoxBmp, storageBmp, floorBmp])
    input
    step