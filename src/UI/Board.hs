module UI.Board where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Sokoban.Data
import Sokoban.Logic

cellSize :: Float
cellSize = 30

backgroundColor :: Color
backgroundColor = black

drawGame :: Board -> IO Picture 
drawGame = return . drawBoard

drawBoard :: Board -> Picture
drawBoard [] = blank
drawBoard (r:rs) = drawRow r <> Translate 0 (-1*cellSize) (drawBoard rs)

drawRow :: Row -> Picture
drawRow [] = blank
drawRow (c:cs) = drawCell c <> Translate cellSize 0 (drawRow cs)

drawCell :: Cell -> Picture
drawCell Wall = drawWall
drawCell (Floor obj) = drawFloor <> drawObject obj
drawCell (Storage obj) = drawStorage <> drawObject obj

drawObject :: Maybe Object -> Picture
drawObject Nothing = blank
drawObject (Just Player) = drawPlayer
drawObject (Just Box) = drawBox

drawFloor :: Picture
drawFloor = color (greyN 0.8) (rectangleSolid cellSize cellSize)

drawWall :: Picture 
drawWall = color (greyN 0.2) (rectangleSolid cellSize cellSize)

drawStorage :: Picture
drawStorage = drawFloor <> color (withAlpha 0.6 green) (circleSolid (cellSize/4))

drawBox :: Picture 
drawBox = color red (rectangleSolid cellSize cellSize)

drawPlayer :: Picture 
drawPlayer = color orange (circleSolid (cellSize/3))