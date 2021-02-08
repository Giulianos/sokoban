module Sokoban.Transitions where

import Graphics.Gloss.Interface.IO.Game
import Sokoban.Data
import Sokoban.Logic

handleInput :: Event -> Board -> Board
handleInput (EventKey (SpecialKey KeyUp) Down _ _) = tryMovePlayer U
handleInput (EventKey (SpecialKey KeyDown) Down _ _) = tryMovePlayer D
handleInput (EventKey (SpecialKey KeyRight) Down _ _) = tryMovePlayer R
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) = tryMovePlayer L
handleInput _ = id