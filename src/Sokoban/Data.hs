module Sokoban.Data where

import Control.Lens.At

data Object = Player | Box deriving (Eq, Show)

data Cell = Floor (Maybe Object)
          | Storage (Maybe Object)
          | Wall deriving (Eq, Show)

type Row    = [Cell]
type Board  = [Row]

type Position = (Index Board, Index Row)

data Direction = U | D | L | R