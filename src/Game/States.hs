module Game.States where

import Sokoban.Data

data Playing = Play Board | Finish Float

data Game = Game { playingState :: Playing
                 , remainingBoards :: [Board]
                 , finishedAllLevels :: Bool
                 }