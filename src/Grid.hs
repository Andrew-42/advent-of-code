module Grid where

import Control.Monad (foldM)
import Data.List (elemIndex, findIndex)
import qualified Utils as U

newtype Grid2D a = Grid2D {getGrid :: [[a]]} deriving (Show, Eq, Ord)

data Position = Position {col :: Int, row :: Int} deriving (Show, Eq, Ord)

data Shape = Shape Int Int deriving (Show, Eq, Ord)

data Direction = North | East | South | West deriving (Show, Eq, Ord)

printGrid :: (Show a) => Grid2D a -> IO ()
printGrid (Grid2D g) = mapM_ print g

-- >>> transpose $ Grid2D [[2],[4]]
-- Grid2D [[2,4]]
transpose :: Grid2D a -> Grid2D a
transpose (Grid2D g) = Grid2D $ U.transpose g

-- >>> shape $ Grid2D [[2],[4]]
-- Shape 2 1
shape :: Grid2D a -> Shape
shape (Grid2D rs) = Shape <$> length . head <*> length $ rs

-- >>> gridAt (Position 0 1) $ Grid2D [[1,2],[3,4]]
-- Just 3
gridAt :: Position -> Grid2D a -> Maybe a
gridAt (Position x y) (Grid2D g) = U.itemAt y g >>= U.itemAt x

-- >>> setGridAt 7 (Position 0 0) $ Grid2D [[1,2],[3,4]]
-- Just (Grid2D [[7,2],[3,4]])
setGridAt :: a -> Position -> Grid2D a -> Maybe (Grid2D a)
setGridAt v (Position x y) (Grid2D m) = do
    row' <- U.itemAt y m
    newRow <- U.setItemAt x v row'
    newGrid <- U.setItemAt y newRow m
    return $ Grid2D newGrid

setAllGridAt :: a -> [Position] -> Grid2D a -> Maybe (Grid2D a)
setAllGridAt v ps g = foldM (flip (setGridAt v)) g ps

-- >>> positionOf 2 $ Grid2D [[1,2],[3,4]]
-- Just (Position 1 0)
positionOf :: (Eq a) => a -> Grid2D a -> Maybe Position
positionOf v (Grid2D rs) = do
    y <- findIndex (elem v) rs
    row' <- U.itemAt y rs
    x <- elemIndex v row'
    return $ Position x y

-- >>> positionsOf 2 (Grid2D [[1,2],[2,4]])
-- [Position 0 1,Position 1 0]
positionsOf :: (Eq a) => a -> Grid2D a -> [Position]
positionsOf c g =
    [ Position x y
    | x <- [0 .. dimX]
    , y <- [0 .. dimY]
    , gridAt (Position x y) g == Just c
    ]
  where
    Shape dimX dimY = shape g

allPositions :: Shape -> [Position]
allPositions (Shape dimX dimY) =
    [Position x y | x <- [0 .. dimX], y <- [0 .. dimY]]

-- >>> isInGrid (Position 1 3) (Shape 2 5)
-- True
isInGrid :: Position -> Shape -> Bool
isInGrid (Position x y) (Shape dimX dimY) =
    (0 <= x && x < dimX) && (0 <= y && y < dimY)

-- >>> countValues 2 (Grid2D [[1,2],[2,4]])
-- 2
countValues :: (Eq a) => a -> Grid2D a -> Int
countValues v (Grid2D rs) = length $ concatMap (filter (== v)) rs

-- >>> directNeighbors (Position 1 1)
-- [Position 0 1,Position 2 1,Position 1 0,Position 1 2]
directNeighbors :: Position -> [Position]
directNeighbors (Position x y) =
    [ Position (x - 1) y
    , Position (x + 1) y
    , Position x (y - 1)
    , Position x (y + 1)
    ]

-- >>> diagNeighbors (Position 1 1)
-- [Position 0 0,Position 2 0,Position 0 2,Position 2 2]
diagNeighbors :: Position -> [Position]
diagNeighbors (Position x y) =
    [ Position (x - 1) (y - 1)
    , Position (x + 1) (y - 1)
    , Position (x - 1) (y + 1)
    , Position (x + 1) (y + 1)
    ]

neighbors :: Position -> [Position]
neighbors = (++) <$> directNeighbors <*> diagNeighbors

areDirectNeighbors :: Position -> Position -> Bool
areDirectNeighbors (Position x1 y1) (Position x2 y2)
    | y1 == y2 = abs (x1 - x2) == 1
    | x1 == x2 = abs (y1 - y2) == 1
    | otherwise = False

-- >>> equals 4 (Position 1 1) (Grid2D [[1,2],[2,4]])
-- True
equals :: (Eq a) => a -> Position -> Grid2D a -> Bool
equals v p g = case gridAt p g of
    Just x -> x == v
    Nothing -> False

compareG :: (a -> Bool) -> Position -> Grid2D a -> Bool
compareG predicate p g = maybe False predicate (gridAt p g)

next :: Position -> Direction -> Position
next (Position x y) North = Position x (y + 1)
next (Position x y) East = Position (x + 1) y
next (Position x y) South = Position x (y - 1)
next (Position x y) West = Position (x - 1) y

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

mkGrid :: a -> Shape -> Grid2D a
mkGrid v (Shape dimX dimY) = Grid2D . replicate dimY . replicate dimX $ v

zeros :: Shape -> Grid2D Int
zeros = mkGrid 0

manhattanDistance :: Position -> Position -> Int
manhattanDistance (Position x1 y1) (Position x2 y2) =
    abs (x1 - x2) + abs (y1 - y2)
