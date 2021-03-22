module Game.UI where

import Sokoban.UI
import Sokoban.Data
import Sokoban.Utils
import Graphics.Gloss
import Game.States

drawCenteredBoard :: [Picture] -> Board -> Picture
drawCenteredBoard pics b = let (rows, cols) = boardSize b
                in Translate ((-1)*(cols*cellSize)/2) ((rows*cellSize)/2) (drawBoard pics b)

drawGame :: [Picture] -> Game -> Picture
drawGame pics g = case playingState g
             of (Play _ b) -> drawCenteredBoard pics b
                (Title _) -> color white (Translate (-75) 0 (Scale 0.2 0.2 (text "Sokoban")))
                (Finish _) -> if finishedAllLevels g then
                                color white (Translate (-200) 0 (Scale 0.2 0.2 (text "Todos los niveles completados!")))
                              else
                                color white (Translate (-115) 0 (Scale 0.2 0.2 (text "Nivel completado!")))
