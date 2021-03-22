module Sokoban.UI where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Sokoban.Data
import Sokoban.Logic

cellSize :: Float
cellSize = 30

drawBoard :: [Picture] -> Board -> Picture
drawBoard pics [] = blank
drawBoard pics (r:rs) = drawRow r pics <> Translate 0 (-1*cellSize) (drawBoard pics rs)

drawRow :: Row -> [Picture] -> Picture
drawRow [] _ = blank
drawRow (c:cs) pics = drawCell c pics <> Translate cellSize 0 (drawRow cs pics)

drawCell :: Cell -> [Picture] -> Picture
drawCell Wall pics = drawWall pics
drawCell (Floor obj) pics = drawFloor pics <> drawObject obj pics
drawCell (Storage (Just Box)) pics = drawStorage pics <> drawBox pics True
drawCell (Storage obj) pics = drawStorage pics <> drawObject obj pics

drawObject :: Maybe Object -> [Picture] -> Picture
drawObject Nothing pics = blank
drawObject (Just Player) pics = drawPlayer pics
drawObject (Just Box) pics = drawBox pics False

drawFloor :: [Picture] -> Picture
drawFloor pics = Scale 0.5 0.5 (pics!!5)

drawWall :: [Picture] -> Picture 
drawWall pics = Scale 0.5 0.5 (pics!!0)

drawStorage :: [Picture] -> Picture
drawStorage pics = Scale 0.5 0.5 (pics!!4)

drawBox :: [Picture] -> Bool -> Picture 
drawBox pics False = Scale 0.5 0.5 (pics!!2)
drawBox pics True = Scale 0.5 0.5 (pics!!3)

drawPlayer :: [Picture] -> Picture 
drawPlayer pics = Scale 0.5 0.5 (pics!!1)

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