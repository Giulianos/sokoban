module Game.Transitions where

import Game.States
import Sokoban.Transitions as Sokoban
import Sokoban.Logic
import Graphics.Gloss.Interface.IO.Game

advanceLevel :: Game -> Game 
advanceLevel game = if null (remainingBoards game) then
                        game{ playingState=Finish 0
                            , finishedAllLevels=True
                            }
                    else
                        let (current:remaining) = remainingBoards game
                        in game{ playingState=Play current
                               , remainingBoards=remaining
                               }

handleInput :: Event -> Game -> Game 
handleInput event game = case playingState game
                         of (Play  b) -> if checkFinishedBoard b then
                                            game { playingState=Finish 3 }
                                         else
                                            game{ playingState=Play (Sokoban.handleInput event b) }
                            (Title timeLeft) -> if timeLeft < 0 then
                                                    advanceLevel game
                                                else
                                                    game
                            (Finish timeLeft) -> if timeLeft < 0 then
                                                    advanceLevel game
                                                 else
                                                     game
                                            
timeStep :: Float -> Game -> Game
timeStep dt game = case playingState game
                   of (Play b) -> game
                      (Finish timeLeft) -> game {playingState=Finish (timeLeft-dt)}
                      (Title timeLeft) -> game {playingState=Title (timeLeft-dt)}