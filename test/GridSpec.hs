{-# OPTIONS_GHC -Wno-type-defaults #-}

module GridSpec where

import Grid (
    Grid2D (Grid2D),
    Position (Position),
    Shape (Shape),
    countValues,
    gridAt,
    isInGrid,
    positionOf,
    positionsOf,
    setGridAt,
    shape,
    transpose,
 )
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)

gridSpec :: Spec
gridSpec = do
    transposeTest
    shapeTest
    gridAtTest
    setGridAtTest
    positionOfTest
    positionsOfTest
    isInGridTest
    countValuesTest

transposeTest :: SpecWith ()
transposeTest = do
    describe "Test transposing a grid." $ do
        it "Transpose a two row grid" $ do
            let grid = Grid2D [[1], [3], [5]]
            transpose grid `shouldBe` Grid2D [[1, 3, 5]]
        it "Transpose a three row grid" $ do
            let grid = Grid2D [[1, 2], [3, 4], [5, 6]]
            transpose grid `shouldBe` Grid2D [[1, 3, 5], [2, 4, 6]]
        it "Transpose a one row grid" $ do
            let grid = Grid2D [[1, 2]]
            transpose grid `shouldBe` Grid2D [[1], [2]]

shapeTest :: SpecWith ()
shapeTest = do
    describe "Test getting shape grid value." $ do
        it "Get shape of two row grid" $ do
            let grid = Grid2D [[1], [3], [5]]
            shape grid `shouldBe` Shape 1 3
        it "Get shape of three row grid" $ do
            let grid = Grid2D [[1, 2], [3, 4], [5, 6]]
            shape grid `shouldBe` Shape 2 3
        it "Get shape of one row grid" $ do
            let grid = Grid2D [[1, 2]]
            shape grid `shouldBe` Shape 2 1

gridAtTest :: SpecWith ()
gridAtTest = do
    describe "Test getting grid value." $ do
        it "Get value at Position 0 0" $ do
            gridAt (Position 0 0) grid `shouldBe` Just 1
        it "Get value at Position 1 1" $ do
            gridAt (Position 1 1) grid `shouldBe` Just 4
        it "Get value at Position 1 2" $ do
            gridAt (Position 1 2) grid `shouldBe` Just 6
        it "Get value at Position 2 2" $ do
            gridAt (Position 2 2) grid `shouldBe` Nothing
  where
    grid = Grid2D [[1, 2], [3, 4], [5, 6]]

setGridAtTest :: SpecWith ()
setGridAtTest = do
    describe "Test setting grid value." $ do
        it "Set value at Position 0 0" $ do
            let expectedGrid1 = Grid2D [[7, 2], [3, 4], [5, 6]]
            setGridAt 7 (Position 0 0) grid `shouldBe` Just expectedGrid1
        it "Set value at Position 0 1" $ do
            let expectedGrid2 = Grid2D [[1, 2], [7, 4], [5, 6]]
            setGridAt 7 (Position 0 1) grid `shouldBe` Just expectedGrid2
        it "Set value at Position 1 2" $ do
            let expectedGrid3 = Grid2D [[1, 2], [3, 4], [5, 7]]
            setGridAt 7 (Position 1 2) grid `shouldBe` Just expectedGrid3
        it "Set value at Position 2 1" $ do
            setGridAt 7 (Position 2 1) grid `shouldBe` Nothing
        it "Set value at Position 1 3" $ do
            setGridAt 7 (Position 1 3) grid `shouldBe` Nothing
  where
    grid = Grid2D [[1, 2], [3, 4], [5, 6]]

positionOfTest :: SpecWith ()
positionOfTest = do
    describe "Test getting position of grid value." $ do
        it "Get position of value 3 in a grid" $ do
            positionOf 3 grid `shouldBe` Just (Position 0 1)
        it "Get position of value 4 in a grid" $ do
            positionOf 4 grid `shouldBe` Just (Position 1 1)
        it "Get position of value 5 in a grid" $ do
            positionOf 5 grid `shouldBe` Just (Position 0 2)
        it "Get position of value 0 in a grid" $ do
            positionOf 0 grid `shouldBe` Nothing
        it "Get position of value 7 in a grid" $ do
            positionOf 7 grid `shouldBe` Nothing
  where
    grid = Grid2D [[1, 2], [3, 4], [5, 6]]

positionsOfTest :: SpecWith ()
positionsOfTest = do
    describe "Test getting positions of grid value." $ do
        it "Get positions of value 1 in a grid" $ do
            positionsOf 1 grid `shouldBe` [Position 0 0, Position 0 2, Position 1 2]
        it "Get positions of value 3 in a grid" $ do
            positionsOf 3 grid `shouldBe` [Position 0 1, Position 1 0]
        it "Get positions of value 4 in a grid" $ do
            positionsOf 4 grid `shouldBe` [Position 1 1]
        it "Get positions of value 2 in a grid" $ do
            positionsOf 2 grid `shouldBe` []
        it "Get positions of value 5 in a grid" $ do
            positionsOf 5 grid `shouldBe` []
  where
    grid = Grid2D [[1, 3], [3, 4], [1, 1]]

isInGridTest :: SpecWith ()
isInGridTest = do
    describe "Test check if position is in grid." $ do
        it "Check that Position 0 0 is in grid" $ do
            isInGrid (Position 0 0) gridShape `shouldBe` True
        it "Check that Position 0 1 is in grid" $ do
            isInGrid (Position 0 1) gridShape `shouldBe` True
        it "Check that Position 1 2 is in grid" $ do
            isInGrid (Position 1 2) gridShape `shouldBe` True
        it "Check that Position 0 3 is in grid" $ do
            isInGrid (Position 0 3) gridShape `shouldBe` False
        it "Check that Position 2 1 is in grid" $ do
            isInGrid (Position 2 1) gridShape `shouldBe` False
  where
    grid = Grid2D [[1, 3], [3, 4], [1, 1]]
    gridShape = shape grid

countValuesTest :: SpecWith ()
countValuesTest = do
    describe "Test counting values in grid." $ do
        it "Count value 1 in grid" $ do
            countValues 1 grid `shouldBe` 3
        it "Count value 3 in grid" $ do
            countValues 3 grid `shouldBe` 2
        it "Count value 4 in grid" $ do
            countValues 4 grid `shouldBe` 1
        it "Count value 2 in grid" $ do
            countValues 2 grid `shouldBe` 0
        it "Count value -1 in grid" $ do
            countValues (-1) grid `shouldBe` 0
  where
    grid = Grid2D [[1, 3], [3, 4], [1, 1]]
