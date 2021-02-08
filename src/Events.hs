module Events ( handleInput
              , step
)
where

import Graphics.Gloss.Interface.IO.Game
import Sokoban.Data
import Sokoban.Logic

handleInput :: Event -> Board -> IO Board
handleInput (EventKey (SpecialKey KeyUp) Down _ _) = return . tryMovePlayer U
handleInput (EventKey (SpecialKey KeyDown) Down _ _) = return . tryMovePlayer D
handleInput (EventKey (SpecialKey KeyRight) Down _ _) = return . tryMovePlayer R
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) = return . tryMovePlayer L
handleInput _ = return

step :: Float -> Board -> IO Board
step = const return