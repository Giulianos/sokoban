module Game.UI where

import Sokoban.UI
import Graphics.Gloss
import Game.States

drawGame :: Game -> Picture
drawGame g = case playingState g
             of (Play b) -> drawBoard b
                (Finish _) -> if finishedAllLevels g then
                                color red (text "Todos los niveles completados!")
                              else
                                color red (text "Nivel completado!")
