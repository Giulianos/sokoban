module Main where

import Events
import BoardUI
import Graphics.Gloss.Interface.IO.Game
import Data
import LevelParser

initialState = parse board "####\n\
                           \#@$.#\n\
                           \#####\n"

main :: IO ()
main =  do
  playIO
    (InWindow "Sokoban" (500, 500) (1, 1))
    backgroundColor
    10
    (fst (head initialState))
    drawGame
    handleInput
    step