module Sokoban.Logic where

import Sokoban.Data
import Control.Lens.At
import Control.Lens
import Control.Lens.Indexed
import Data.Maybe

objectAt :: Cell -> Maybe Object
objectAt (Floor obj) = obj
objectAt (Storage obj) = obj
objectAt Wall = Nothing

emptyCell :: Cell -> Cell
emptyCell (Floor _) = Floor Nothing
emptyCell (Storage _) = Storage Nothing
emptyCell Wall = Wall

putObjectInCell :: Cell -> Object -> Cell
putObjectInCell (Floor Nothing) = Floor . Just
putObjectInCell (Storage Nothing) = Storage . Just
putObjectInCell c = const c

canMoveObjectTo :: Cell -> Bool
canMoveObjectTo (Floor Nothing) = True
canMoveObjectTo (Storage Nothing) = True
canMoveObjectTo _ = False

moveObject :: (Cell, Cell) -> (Cell, Cell)
moveObject (src, dst) = let srcObj = objectAt src
                        in
                            if canMoveObjectTo dst then
                                (emptyCell src, maybe dst (putObjectInCell dst) srcObj)
                            else
                                (src, dst)

pairMaybes :: Maybe a -> Maybe b -> Maybe (a, b)
pairMaybes mx my = do x <- mx; y <- my; return (x, y)

directionDelta :: Direction -> (Int, Int)
directionDelta U = (-1, 0)
directionDelta D = (1, 0)
directionDelta L = (0, -1)
directionDelta R = (0, 1)

translatePos :: Position -> Direction -> Position
translatePos (r, c) dir = let (dr, dc) = directionDelta dir in (r + dr, c + dc)

cellAt :: Position -> Board -> Maybe Cell
cellAt (row, col) = preview (ix row . ix col)

setCellAt :: Position -> Cell -> Board -> Board
setCellAt (row, col) = set (ix row . ix col)

translateObject :: Position -> Direction -> Board -> Board
translateObject pos dir board = let pos' = translatePos pos dir
                                    cells = pairMaybes (cellAt pos board) (cellAt pos' board)
                                in 
                                    maybe board
                                          (\cs -> let (src', dst') = moveObject cs in setCellAt pos src' (setCellAt pos' dst' board))
                                          cells

cellHasPlayer :: Cell -> Bool
cellHasPlayer (Floor (Just Player)) = True
cellHasPlayer (Storage (Just Player)) = True
cellHasPlayer _ = False

findPlayer :: Board -> Maybe Position
findPlayer b = do
                (row, mcells) <- ifind (const isJust) (map (ifind (const cellHasPlayer)) b)
                (col, mcell) <- mcells
                return (row, col)

movePlayerAndPush :: Position -> Direction -> Board -> Board
movePlayerAndPush player dir board = let dst = translatePos player dir
                                         board' = translateObject dst dir board
                                     in translateObject player dir board'

tryMovePlayer :: Direction -> Board -> Board
tryMovePlayer dir board = maybe board (\playerPos -> movePlayerAndPush playerPos dir board) (findPlayer board)

isBoxMisplaced :: Cell -> Bool
isBoxMisplaced (Floor (Just Box)) =True
isBoxMisplaced _ = False

isRowUnfinished :: Row -> Bool
isRowUnfinished = foldl (flip $ (||) . isBoxMisplaced) False

checkFinishedBoard :: Board -> Bool
checkFinishedBoard = not . (foldl (flip $ (||) . isRowUnfinished) False)