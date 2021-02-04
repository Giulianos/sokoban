module UI
    ( drawGame
    , handleInput
    , stepGame
    )
    where

import Game ( State(player, level), Cell(..), Position, Row, Level, rows, cols )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

cellSize = 30

drawGame :: State -> IO Picture
drawGame state = return (levelPicture <> playerPicture)
 where
  levelPicture = drawLevel $ level state
  playerPicture = drawPlayer


drawLevel :: Level -> Picture 
drawLevel = snd . foldl (\(y, pic) row -> (y - cellSize, pic <> Translate 0 y (drawRow row))) (0, Blank)

drawRow :: Row -> Picture
drawRow = snd . foldl (\(x, pic) cell -> (x + cellSize, pic <> Translate x 0 (drawCell cell))) (0, Blank )

drawCell :: Cell -> Picture
drawCell c = case c of
          Wall -> drawWall
          Floor -> drawFloor
          Storage  -> drawStorage

drawPlayer :: Picture
drawPlayer = color black (rectangleSolid cellSize cellSize)

drawFloor :: Picture
drawFloor = color (greyN 40) (rectangleSolid cellSize cellSize)

drawWall :: Picture
drawWall = color orange (rectangleSolid cellSize cellSize)

drawStorage :: Picture
drawStorage = color red (rectangleSolid cellSize cellSize)

convertPosition :: Position -> (Float, Float)
convertPosition (row, col) = (fromIntegral row * cellSize * (-1), fromIntegral col * cellSize)

handleInput :: Event -> State -> IO State
handleInput event = return

stepGame :: Float -> State -> IO State
stepGame _ = return