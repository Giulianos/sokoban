module UI
    ( drawGame
    , handleInput
    , stepGame
    , backgroundColor
    )
    where

import Game ( State(..), Cell(..), Direction(..), Position, Row, Level, rows, cols, transition )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

cellSize = 30

centerBoard :: State -> Picture -> Picture
centerBoard s p =
  let dx = (-1 * cellSize/2) * fromIntegral (cols (level s))
      dy = (cellSize / 2) * fromIntegral (rows (level s))
  in
    Translate dx dy p 

-- Drawing functions

drawGame :: State -> IO Picture
drawGame state = return
  (
    boardPicture <>
    finishedPicture
  )
 where
   boardPicture = drawBoard state
   finishedPicture = if finished state then drawFinished else Blank

drawBoard :: State -> Picture
drawBoard state =
  let levelPicture = drawLevel $ level state
      playerPicture = drawPlayer $ player state
      boxesPicture = Pictures (map drawBox (boxes state))
  in
    centerBoard state (levelPicture <> playerPicture <> boxesPicture)

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

drawFinished :: Picture 
drawFinished = Translate (-170) 0 (Scale 0.25 0.25 (color black (Text "Terminaste el nivel!")))

-- Colors:
backgroundColor :: Color
backgroundColor = greyN 0.2

convertPosition :: Position -> (Float, Float)
convertPosition (row, col) = (fromIntegral col * cellSize, fromIntegral row * cellSize * (-1))

eventToTransition :: Event -> (State -> State)
eventToTransition (EventKey (SpecialKey KeyUp) Down _ _) = transition MUp
eventToTransition (EventKey (SpecialKey KeyDown) Down _ _) = transition MDown
eventToTransition (EventKey (SpecialKey KeyRight) Down _ _) = transition MRight
eventToTransition (EventKey (SpecialKey KeyLeft) Down _ _) = transition MLeft
eventToTransition _ = id

handleInput :: Event -> State -> IO State
handleInput e s =
  if finished s then
    return s
  else
    return (eventToTransition e s)

stepGame :: Float -> State -> IO State
stepGame _ = return