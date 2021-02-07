module Main where

import Events
import BoardUI
import Graphics.Gloss.Interface.IO.Game
import Data

initialState = [
  [Wall, Wall,          Wall,          Wall],
  [Wall, Floor Nothing, Floor Nothing, Wall],
  [Wall, Floor Nothing, Floor (Just Box), Wall],
  [Wall, Floor (Just Player), Floor Nothing, Wall],
  [Wall, Floor Nothing, Floor (Just Box), Wall],
  [Wall, Floor Nothing, Storage Nothing, Wall],
  [Wall, Wall,          Wall,          Wall]]

main :: IO ()
main =  do
  playIO
    (InWindow "Sokoban" (500, 500) (1, 1))
    backgroundColor
    10
    initialState
    drawGame
    handleInput
    step