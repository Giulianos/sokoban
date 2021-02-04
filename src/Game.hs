module Game
    ( State(..)
    , Cell(..)
    , Position
    , Row
    , Level
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

data State = State { level :: Level
                   , player :: Position
                   , boxes :: [Position]
                   } deriving (Show)

initialState :: State
initialState = State {
    level=[
        [ Wall, Wall,  Wall,  Wall,    Wall ],
        [ Wall, Floor, Floor, Storage, Wall ],
        [ Wall, Floor, Floor, Floor,   Wall ],
        [ Wall, Floor, Floor, Floor,   Wall ],
        [ Wall, Floor, Floor, Storage, Wall ],
        [ Wall, Wall,  Wall,  Wall,    Wall ]
    ],
    player=(1,1),
    boxes=[]
}