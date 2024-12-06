module Grid where

import Data.List (elemIndex, findIndex)
import Utils (itemAt, setItemAt)

newtype Grid2D a = Grid2D [[a]] deriving (Show, Eq, Ord)

data Position = Position Int Int deriving (Show, Eq, Ord)

shape :: Grid2D a -> (Int, Int)
shape (Grid2D rs) = (length rs, length (head rs))

gridAt :: Position -> Grid2D a -> Maybe a
gridAt (Position x y) (Grid2D g) = itemAt y g >>= itemAt x

setGridAt :: a -> Position -> Grid2D a -> Maybe (Grid2D a)
setGridAt v (Position x y) (Grid2D m) = do
    row <- itemAt y m
    newRow <- setItemAt x v row
    newGrid <- setItemAt y newRow m
    return $ Grid2D newGrid

positionOf :: (Eq a) => a -> Grid2D a -> Maybe Position
positionOf v (Grid2D rs) = do
    y <- findIndex (elem v) rs
    row <- itemAt y rs
    x <- elemIndex v row
    return $ Position x y

countValues :: (Eq a) => a -> Grid2D a -> Int
countValues v (Grid2D rs) = length $ concatMap (filter (== v)) rs
