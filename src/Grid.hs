module Grid where

import Data.List (elemIndex, findIndex)
import qualified Utils as U

newtype Grid2D a = Grid2D [[a]] deriving (Show, Eq, Ord)

data Position = Position Int Int deriving (Show, Eq, Ord)

data Shape = Shape Int Int deriving (Show, Eq, Ord)

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
    row <- U.itemAt y m
    newRow <- U.setItemAt x v row
    newGrid <- U.setItemAt y newRow m
    return $ Grid2D newGrid

-- >>> positionOf 2 $ Grid2D [[1,2],[3,4]]
-- Just (Position 1 0)
positionOf :: (Eq a) => a -> Grid2D a -> Maybe Position
positionOf v (Grid2D rs) = do
    y <- findIndex (elem v) rs
    row <- U.itemAt y rs
    x <- elemIndex v row
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

-- >>> isInGrid (Position 1 3) (Shape 2 5)
-- True
isInGrid :: Position -> Shape -> Bool
isInGrid (Position x y) (Shape dimX dimY) =
    (0 <= x && x < dimX) && (0 <= y && y < dimY)

-- >>> countValues 2 (Grid2D [[1,2],[2,4]])
-- 2
countValues :: (Eq a) => a -> Grid2D a -> Int
countValues v (Grid2D rs) = length $ concatMap (filter (== v)) rs
