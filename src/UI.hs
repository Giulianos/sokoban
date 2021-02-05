module UI
    ( drawGame
    , handleInput
    , stepGame
    , backgroundColor
    )
    where

import Game ( State(player, level, boxes), Cell(..), Direction(..), Position, Row, Level, rows, cols, transition )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

cellSize = 30

pictureCenter :: State -> (Float, Float)
pictureCenter s = ((-1 * cellSize/2) * (fromIntegral $ cols (level s)), (cellSize / 2) * (fromIntegral $ rows (level s)))

-- Drawing functions

drawGame :: State -> IO Picture
drawGame state = return ((uncurry Translate) (pictureCenter state) (levelPicture <> playerPicture <> boxesPicture))
 where
  levelPicture = drawLevel $ level state
  playerPicture = drawPlayer $ player state
  boxesPicture = Pictures (map drawBox (boxes state))

drawLevel :: Level -> Picture 
drawLevel = snd . foldl (\(y, pic) row -> (y - cellSize, pic <> Translate 0 y (drawRow row))) (0, Blank)

drawRow :: Row -> Picture
drawRow = snd . foldl (\(x, pic) cell -> (x + cellSize, pic <> Translate x 0 (drawCell cell))) (0, Blank )

drawCell :: Cell -> Picture
drawCell c = case c of
          Wall -> drawWall
          Floor -> drawFloor
          Storage  -> drawStorage

drawPlayer :: Position -> Picture
drawPlayer p = uncurry Translate (convertPosition p) (color aquamarine (circleSolid (cellSize/3)))

drawFloor :: Picture
drawFloor = color (greyN 0.8) (rectangleSolid cellSize cellSize)

drawWall :: Picture
drawWall = color (greyN 0.5) (rectangleSolid cellSize cellSize)

drawStorage :: Picture
drawStorage = color red (rectangleSolid cellSize cellSize)

drawBox :: Position -> Picture
drawBox b = uncurry Translate (convertPosition b) (color azure (rectangleSolid (0.8*cellSize) (0.8*cellSize))) 


-- Colors:

backgroundColor :: Color
backgroundColor = greyN 0.2

-- TODO: fix
convertPosition :: Position -> (Float, Float)
convertPosition (row, col) = (fromIntegral col * cellSize, fromIntegral row * cellSize * (-1))

handleInput :: Event -> State -> IO State
handleInput (EventKey (SpecialKey KeyUp) Down _ _) = return . (transition MUp)
handleInput (EventKey (SpecialKey KeyDown) Down _ _) = return . (transition MDown)
handleInput (EventKey (SpecialKey KeyRight) Down _ _) = return . (transition MRight)
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) = return . (transition MLeft)
handleInput _ = return

stepGame :: Float -> State -> IO State
stepGame _ = return