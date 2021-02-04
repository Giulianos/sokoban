module Main where
import Graphics.Gloss.Interface.IO.Game
import Game
import UI

main :: IO ()
main =  do
  playIO
    (InWindow "Sokoban" (500, 500) (1, 1))
    azure
    10
    initialState
    drawGame
    handleInput
    stepGame