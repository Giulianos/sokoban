module Game.States where

import Sokoban.Data

data Playing = Play Board Board | Finish Float | Title Float

data Game = Game { playingState :: Playing
                 , remainingBoards :: [Board]
                 , finishedAllLevels :: Bool
                 }