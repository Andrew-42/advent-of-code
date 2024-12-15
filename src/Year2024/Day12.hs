module Year2024.Day12 (solve) where

import Data.List (sort)
import qualified Data.Set as S
import Debug.Trace (trace)
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
import Utils (dropLast, takeLast)

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
-- >>> part2 example

>>> let example = "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"
>>> extractPlots . Grid2D . lines $ example
[GardenPlot (Position 9 9) (fromList [Position 7 8,Position 7 9,Position 8 5,Position 8 6,Position 8 7,Position 8 8,Position 8 9,Position 9 4,Position 9 5,Position 9 6,Position 9 7,Position 9 8,Position 9 9]) 'E',GardenPlot (Position 7 4) (fromList [Position 7 4]) 'C',GardenPlot (Position 9 3) (fromList [Position 7 2,Position 7 3,Position 8 0,Position 8 2,Position 8 3,Position 8 4,Position 9 0,Position 9 1,Position 9 2,Position 9 3]) 'F',GardenPlot (Position 7 7) (fromList [Position 5 4,Position 6 3,Position 6 4,Position 6 5,Position 6 6,Position 6 7,Position 6 8,Position 6 9,Position 7 5,Position 7 6,Position 7 7]) 'J',GardenPlot (Position 5 9) (fromList [Position 4 8,Position 4 9,Position 5 9]) 'S',GardenPlot (Position 5 1) (fromList [Position 4 0,Position 4 1,Position 5 0,Position 5 1]) 'I',GardenPlot (Position 8 1) (fromList [Position 3 3,Position 4 3,Position 4 4,Position 4 5,Position 5 2,Position 5 3,Position 5 5,Position 5 6,Position 6 0,Position 6 1,Position 6 2,Position 7 0,Position 7 1,Position 8 1]) 'C',GardenPlot (Position 5 8) (fromList [Position 1 7,Position 1 8,Position 2 5,Position 2 6,Position 2 7,Position 2 8,Position 3 6,Position 3 7,Position 3 8,Position 3 9,Position 4 6,Position 4 7,Position 5 7,Position 5 8]) 'I',GardenPlot (Position 2 9) (fromList [Position 0 7,Position 0 8,Position 0 9,Position 1 9,Position 2 9]) 'M',GardenPlot (Position 3 5) (fromList [Position 0 2,Position 0 3,Position 0 4,Position 0 5,Position 0 6,Position 1 2,Position 1 3,Position 1 4,Position 1 5,Position 1 6,Position 2 4,Position 3 4,Position 3 5]) 'V',GardenPlot (Position 4 2) (fromList [Position 0 0,Position 0 1,Position 1 0,Position 1 1,Position 2 0,Position 2 1,Position 2 2,Position 2 3,Position 3 0,Position 3 1,Position 3 2,Position 4 2]) 'R']
-}
part2 :: String -> Int
part2 =
    sum . map (\gp -> area gp * countSides gp) . extractPlots . Grid2D . lines

data Direction = U | D | L | R deriving (Show, Eq, Ord)

data Side = Side Position Direction deriving (Show, Eq, Ord)

-- >>> countSides (GardenPlot (Position 4 2) (S.fromList [Position 0 0,Position 0 1,Position 1 0,Position 1 1,Position 2 0,Position 2 1,Position 2 2,Position 2 3,Position 3 0,Position 3 1,Position 3 2,Position 4 2]) 'R')
-- 18
countSides :: GardenPlot -> Int
countSides gp = nu + nd + nl + nr
  where
    allS = allPositionSides gp
    nu = length . groupSide U $ allS
    nd = length . groupSide D $ allS
    nl = length . groupSide L $ allS
    nr = length . groupSide R $ allS

-- >>> groupSide D [Side (Position 0 1) U,Side (Position 1 0) R,Side (Position 0 0) D,Side (Position 1 1) R,Side (Position 1 1) U,Side (Position 0 0) L,Side (Position 2 0) R,Side (Position 1 0) D,Side (Position 0 1) L,Side (Position 2 1) R,Side (Position 2 1) U,Side (Position 1 0) L,Side (Position 3 0) R,Side (Position 2 2) U,Side (Position 2 0) D,Side (Position 1 1) L,Side (Position 3 1) R,Side (Position 2 3) U,Side (Position 2 1) D,Side (Position 3 2) R,Side (Position 2 2) D,Side (Position 3 1) U,Side (Position 2 0) L,Side (Position 3 2) U,Side (Position 3 0) D,Side (Position 2 1) L,Side (Position 3 1) D,Side (Position 2 2) L,Side (Position 4 2) R,Side (Position 3 2) L]
-- [[Side (Position 0 0) D,Side (Position 1 0) D,Side (Position 2 0) D],[Side (Position 2 1) D],[Side (Position 2 2) D],[Side (Position 3 0) D],[Side (Position 3 1) D]]
groupSide :: Direction -> [Side] -> [[Side]]
groupSide _ [] = []
groupSide d ss = flip concatSides [[]] . sort . filter (isDirection d) $ ss
  where
    concatSides :: [Side] -> [[Side]] -> [[Side]]
    concatSides [] res = res
    concatSides (s : rs) [[]] = concatSides rs [[s]]
    concatSides (s : rs) res =
        if last (last res) `isNeighbor` s
            then concatSides rs (dropLast 1 res ++ [last res ++ [s]])
            else concatSides rs (res ++ [[s]])

isDirection :: Direction -> Side -> Bool
isDirection d (Side _ sd) = d == sd

-- groupSide (x:xs) rs
--   | any isNeighbor x rs = sort groupSide

isNeighbor :: Side -> Side -> Bool
isNeighbor (Side (Position x1 y1) d1) (Side (Position x2 y2) d2)
    | d1 == d2 = case d1 of
        U -> (y1 == y2) && (abs (x1 - x2) == 1)
        D -> (y1 == y2) && (abs (x1 - x2) == 1)
        L -> (x1 == x2) && (abs (y1 - y2) == 1)
        R -> (x1 == x2) && (abs (y1 - y2) == 1)
    | otherwise = False

-- >>> length $ allPositionSides (GardenPlot (Position 4 2) (S.fromList [Position 0 0,Position 0 1,Position 1 0,Position 1 1,Position 2 0,Position 2 1,Position 2 2,Position 2 3,Position 3 0,Position 3 1,Position 3 2,Position 4 2]) 'R')
-- 30
allPositionSides :: GardenPlot -> [Side]
allPositionSides (GardenPlot _ ps _) = concatMap pSides ps
  where
    -- TODO: fix filtering
    pSides p = filter (\(Side sp _) -> sp `elem` ps) $ positionSides p

positionSides :: Position -> [Side]
positionSides p = [Side p U, Side p D, Side p L, Side p R]

solve :: IO ()
solve = do
    content <- readFile "./src/Year2024/data/day12.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
