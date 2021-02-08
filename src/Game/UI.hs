module Game.UI where

import Sokoban.UI
import Graphics.Gloss
import Game.States

drawGame :: Game -> Picture
drawGame g = case playingState g
             of (Play b) -> drawBoard b
                (Title _) -> color white (Translate (-75) 0 (Scale 0.2 0.2 (text "Sokoban")))
                (Finish _) -> if finishedAllLevels g then
                                color white (Translate (-200) 0 (Scale 0.2 0.2 (text "Todos los niveles completados!")))
                              else
                                color white (Translate (-115) 0 (Scale 0.2 0.2 (text "Nivel completado!")))
