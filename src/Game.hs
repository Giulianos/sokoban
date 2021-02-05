{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
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
                   , finished :: Bool
                   } deriving (Show)

data Direction = MUp | MDown | MLeft | MRight

-- State transitions
transition :: Direction -> State -> State
transition dir s =
    let p'  = transitionPlayer dir s
        bs' = transitionBoxes dir s
    in
        State {
            level=level s,
            player=p',
            boxes=bs',
            finished=isFinished (level s) bs'
        }

transitionPlayer :: Direction -> State -> Position
transitionPlayer d s = 
    let p = player s
        p' = move d (player s)
    in
        if isWalkable (level s) p' && (not (hasBoxAt s p') || canPush s d p') then
            p'
        else
            p

transitionBoxes :: Direction -> State -> [Position]
transitionBoxes d s = map (\box -> if isTouching d (player s) box && canPush s d box then move d box else box) (boxes s)

isWalkableRow :: Row -> Position -> Bool
isWalkableRow [] _ = False
isWalkableRow (Wall:cs) (_, 0) = False
isWalkableRow (_:cs) (_, 0) = True
isWalkableRow (c:cs) (row, col) = isWalkableRow cs (row, col - 1)

isWalkable :: Level -> Position -> Bool
isWalkable [] _ = False
isWalkable (r:rs) (0, col) = isWalkableRow r (0, col)
isWalkable (r:rs) (row, col) = isWalkable rs (row - 1, col)

hasBoxAt :: State -> Position -> Bool
hasBoxAt s p = p `elem` boxes s

isEmpty :: State -> Position -> Bool
isEmpty s p = not (hasBoxAt s p) && isWalkable (level s) p

isTouching :: Direction -> Position -> Position -> Bool
isTouching MUp (r1, c1) (r2, c2)    = (c1 == c2) && (r2 == (r1 - 1))
isTouching MDown (r1, c1) (r2, c2)  = (c1 == c2) && (r2 == (r1 + 1))
isTouching MLeft (r1, c1) (r2, c2)  = (c2 == (c1 - 1)) && (r1 == r2)
isTouching MRight (r1, c1) (r2, c2) = (c2 == (c1 + 1)) && (r1 == r2)
isTouching _ _ _ = False

isAtStorageRow :: Row -> Position -> Bool
isAtStorageRow [] _ = False
isAtStorageRow (Storage:cs) (_, 0) = True
isAtStorageRow (_:cs) (_, 0) = False
isAtStorageRow (c:cs) (row, col) = isAtStorageRow cs (row, col - 1)

isAtStorage :: Level -> Position -> Bool
isAtStorage [] _ = False
isAtStorage (r:rs) (0, col) = isAtStorageRow r (0, col)
isAtStorage (r:rs) (row, col) = isAtStorage rs (row - 1, col)

isFinished :: Level -> [Position] -> Bool
isFinished l = foldl (\allAtStorage box -> allAtStorage && isAtStorage l box) True

canPush :: State -> Direction -> Position -> Bool
canPush s d = isEmpty s . move d

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
    boxes=[(2,2), (3,2)],
    finished = False
}