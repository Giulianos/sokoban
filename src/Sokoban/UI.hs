module Sokoban.UI where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Sokoban.Data
import Sokoban.Logic

cellSize :: Float
cellSize = 30

drawBoard :: Board -> Picture
drawBoard [] = blank
drawBoard (r:rs) = drawRow r <> Translate 0 (-1*cellSize) (drawBoard rs)

drawRow :: Row -> Picture
drawRow [] = blank
drawRow (c:cs) = drawCell c <> Translate cellSize 0 (drawRow cs)

drawCell :: Cell -> Picture
drawCell Wall = drawWall
drawCell (Floor obj) = drawFloor <> drawObject obj
drawCell (Storage (Just Box)) = drawStorage <> drawBox True
drawCell (Storage obj) = drawStorage <> drawObject obj

drawObject :: Maybe Object -> Picture
drawObject Nothing = blank
drawObject (Just Player) = drawPlayer
drawObject (Just Box) = drawBox False

drawFloor :: Picture
drawFloor = color floorColor (rectangleSolid cellSize cellSize)

drawWall :: Picture 
drawWall = color wallColor (rectangleSolid cellSize cellSize)

drawStorage :: Picture
drawStorage = drawFloor <> color storageColor (circleSolid (cellSize/4))

drawBox :: Bool -> Picture 
drawBox stored = color (boxColor stored) (rectangleSolid cellSize cellSize)

drawPlayer :: Picture 
drawPlayer = color playerColor (circleSolid (cellSize/3))

-- Colors:
floorColor :: Color
floorColor = makeColorI 234 234 234 255

wallColor :: Color
wallColor = makeColorI 40 114 155 255

playerColor :: Color
playerColor = makeColorI 121 78 154 255

boxColor :: Bool -> Color
boxColor False = makeColorI 152 70 70 255
boxColor True = makeColorI 42 139 110 255

storageColor :: Color
storageColor = makeColorI 196 196 196 255