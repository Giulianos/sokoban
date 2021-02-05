module Game
    ( State(..)
    , Cell(..)
    , Position
    , Row
    , Level
    , Direction(..)
    , transition
    , initialState
    , cols
    , rows
    ) where

data Cell = Floor | Wall | Storage deriving Show
type Row = [ Cell ]
type Level = [ Row ]

rows :: Level -> Int
rows = length

cols :: Level -> Int
cols = foldl (\currLen list -> let listLen = length list in (if listLen > currLen then listLen else currLen)) 0

type Position = (Int, Int)
add :: Position -> Position -> Position
add (r1, c1) (r2, c2) = (r1+r2, c1+c2)

data State = State { level :: Level
                   , player :: Position
                   , boxes :: [Position]
                   } deriving (Show)

data Direction = MUp | MDown | MLeft | MRight

-- State transitions
transition :: Direction -> State -> State
transition dir s = State {
    level=level s,
    player=
        let pos    = (player s)
            newPos = move dir (player s)
        in
            if (isWalkable (level s) newPos) then
                newPos
            else
                pos
        ,
    boxes=boxes s
}

isWalkableRow :: Row -> Position -> Bool
isWalkableRow [] _ = False
isWalkableRow (Wall:cs) (_, 0) = False
isWalkableRow (_:cs) (_, 0) = True
isWalkableRow (c:cs) (row, col) = isWalkableRow cs (row, col - 1)

isWalkable :: Level -> Position -> Bool
isWalkable [] _ = False
isWalkable (r:rs) (0, col) = isWalkableRow r (0, col)
isWalkable (r:rs) (row, col) = isWalkable rs (row - 1, col)

move :: Direction -> Position -> Position
move MUp    = add (-1, 0)
move MDown  = add (1, 0)
move MLeft  = add (0, -1)
move MRight = add (0, 1)

initialState :: State
initialState = State {
    level=[
        [ Wall, Wall,  Wall,  Wall,  Wall,  Wall,  Wall,    Wall ],
        [ Wall, Floor, Floor, Floor, Floor, Floor, Storage, Wall ],
        [ Wall, Floor, Floor, Floor, Floor, Floor, Floor,   Wall ],
        [ Wall, Floor, Floor, Floor, Floor, Floor, Floor,   Wall ],
        [ Wall, Floor, Floor, Floor, Floor, Floor, Wall,    Wall ],
        [ Wall, Floor, Floor, Floor, Floor, Floor, Storage, Wall ],
        [ Wall, Floor, Floor, Floor, Floor, Floor, Wall,    Wall ],
        [ Wall, Floor, Floor, Floor, Floor, Floor, Floor,   Wall ],
        [ Wall, Floor, Floor, Floor, Floor, Floor, Floor,   Wall ],
        [ Wall, Wall,  Wall,  Wall,  Wall,  Wall,  Wall,    Wall ]
    ],
    player=(2,1),
    boxes=[(2,2), (3,2)]
}