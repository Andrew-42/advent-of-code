module Year2024.Day12 (solve) where

import qualified Data.Set as S
import Grid (
    Grid2D (Grid2D),
    Position (Position),
    allPositions,
    areDirectNeighbors,
    directNeighbors,
    equals,
    gridAt,
    shape,
 )

{- | Part 1

Example should be 1930:

>>> let example = "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"
>>> part1 example
1930
-}
part1 :: String -> Int
part1 =
    sum . map (\gp -> area gp * perimeter gp) . extractPlots . Grid2D . lines

data GardenPlot = GardenPlot Position (S.Set Position) Char deriving (Show, Eq)

area :: GardenPlot -> Int
area (GardenPlot _ ps _) = length ps

perimeter :: GardenPlot -> Int
perimeter (GardenPlot _ ps _) =
    sum $ map (\p -> 4 - neighborCount p) $ S.toList ps
  where
    neighborCount p = S.size $ S.filter (areDirectNeighbors p) ps

-- >>> let example = "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"
-- >>> extractPlots . Grid2D . lines $ example
-- [GardenPlot (Position 9 9) (fromList [Position 7 8,Position 7 9,Position 8 5,Position 8 6,Position 8 7,Position 8 8,Position 8 9,Position 9 4,Position 9 5,Position 9 6,Position 9 7,Position 9 8,Position 9 9]) 'E',GardenPlot (Position 7 4) (fromList [Position 7 4]) 'C',GardenPlot (Position 9 3) (fromList [Position 7 2,Position 7 3,Position 8 0,Position 8 2,Position 8 3,Position 8 4,Position 9 0,Position 9 1,Position 9 2,Position 9 3]) 'F',GardenPlot (Position 7 7) (fromList [Position 5 4,Position 6 3,Position 6 4,Position 6 5,Position 6 6,Position 6 7,Position 6 8,Position 6 9,Position 7 5,Position 7 6,Position 7 7]) 'J',GardenPlot (Position 5 9) (fromList [Position 4 8,Position 4 9,Position 5 9]) 'S',GardenPlot (Position 5 1) (fromList [Position 4 0,Position 4 1,Position 5 0,Position 5 1]) 'I',GardenPlot (Position 8 1) (fromList [Position 3 3,Position 4 3,Position 4 4,Position 4 5,Position 5 2,Position 5 3,Position 5 5,Position 5 6,Position 6 0,Position 6 1,Position 6 2,Position 7 0,Position 7 1,Position 8 1]) 'C',GardenPlot (Position 5 8) (fromList [Position 1 7,Position 1 8,Position 2 5,Position 2 6,Position 2 7,Position 2 8,Position 3 6,Position 3 7,Position 3 8,Position 3 9,Position 4 6,Position 4 7,Position 5 7,Position 5 8]) 'I',GardenPlot (Position 2 9) (fromList [Position 0 7,Position 0 8,Position 0 9,Position 1 9,Position 2 9]) 'M',GardenPlot (Position 3 5) (fromList [Position 0 2,Position 0 3,Position 0 4,Position 0 5,Position 0 6,Position 1 2,Position 1 3,Position 1 4,Position 1 5,Position 1 6,Position 2 4,Position 3 4,Position 3 5]) 'V',GardenPlot (Position 4 2) (fromList [Position 0 0,Position 0 1,Position 1 0,Position 1 1,Position 2 0,Position 2 1,Position 2 2,Position 2 3,Position 3 0,Position 3 1,Position 3 2,Position 4 2]) 'R']
extractPlots :: Grid2D Char -> [GardenPlot]
extractPlots g = extract ps []
  where
    ps = S.fromList . allPositions . shape $ g

    extract :: S.Set Position -> [GardenPlot] -> [GardenPlot]
    extract ps' store
        | S.null ps' = store
        | otherwise = case gardenPlot seedP g of
            Just gp@(GardenPlot _ gps _) ->
                extract (S.difference ps' gps) (gp : store)
            Nothing -> extract (S.delete seedP ps') store
      where
        seedP = S.elemAt 0 ps'

gardenPlot :: Position -> Grid2D Char -> Maybe GardenPlot
gardenPlot p g = do
    plant <- gridAt p g
    let ps = expand plant (S.singleton p) (S.singleton p)
    maxP <- S.lookupMax ps
    return $ GardenPlot maxP ps plant
  where
    expand :: Char -> S.Set Position -> S.Set Position -> S.Set Position
    expand c frontier store
        | S.null frontier = store
        | otherwise = expand c newFrontier (S.union store newFrontier)
      where
        newFrontier =
            S.filter (\p -> S.notMember p store && equals c p g)
                . S.fromList
                . concatMap directNeighbors
                . S.toList
                $ frontier

{- | Part 2

Example:

>>> let example = "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"
>>> part2 example
-}
part2 :: String -> Int
part2 = undefined

data Direction = U | D | L | R deriving (Show, Eq)

data Side = Side Position Direction deriving (Show, Eq)

-- >>> let plot =  GardenPlot (Position 5 1) (S.fromList [Position 4 0,Position 4 1,Position 5 0,Position 5 1]) 'I'
-- >>> allSides plot
-- [Side (Position 4 1) U,Side (Position 5 0) R,Side (Position 4 0) D,Side (Position 5 1) R,Side (Position 5 1) U,Side (Position 4 0) L,Side (Position 5 0) D,Side (Position 4 1) L]
allSides :: GardenPlot -> [Side]
allSides (GardenPlot _ ps _) = concatMap pSides ps
  where
    pSides p = filter (\(Side sp _) -> sp `elem` ps) $ positionSides p

positionSides :: Position -> [Side]
positionSides (Position x y) =
    [ Side (Position x (y + 1)) U
    , Side (Position x (y - 1)) D
    , Side (Position (x - 1) y) L
    , Side (Position (x + 1) y) R
    ]

solve :: IO ()
solve = do
    content <- readFile "./src/Year2024/data/day12.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
