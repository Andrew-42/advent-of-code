module Year2024.Day12 (solve) where

import Data.List (sortOn)
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
import Utils (dropLast)

{- | Part 1

Why not search for the Chief Historian near the gardener and his massive farm?
There's plenty of food, so The Historians grab something to eat while they
search.

You're about to settle near a complex arrangement of garden plots when some
Elves ask if you can lend a hand. They'd like to set up fences around each
region of garden plots, but they can't figure out how much fence they need to
order or how much it will cost. They hand you a map (your puzzle input) of the
garden plots.

Each garden plot grows only a single type of plant and is indicated by a single
letter on your map. When multiple garden plots are growing the same type of
plant and are touching (horizontally or vertically), they form a region.
For example:

AAAA
BBCD
BBCC
EEEC

This 4x4 arrangement includes garden plots growing five different types of
plants (labeled A, B, C, D, and E), each grouped into their own region.

In order to accurately calculate the cost of the fence around a single region,
you need to know that region's area and perimeter.

The area of a region is simply the number of garden plots the region contains.
The above map's type A, B, and C plants are each in a region of area 4. The type
E plants are in a region of area 3; the type D plants are in a region of area 1.

Each garden plot is a square and so has four sides. The perimeter of a region is
the number of sides of garden plots in the region that do not touch another
garden plot in the same region. The type A and C plants are each in a region
with perimeter 10. The type B and E plants are each in a region with perimeter
8. The lone D plot forms its own region with perimeter 4.

Visually indicating the sides of plots in each region that contribute to the
perimeter using - and |, the above map's regions' perimeters are measured as
follows:

+-+-+-+-+
|A A A A|
+-+-+-+-+     +-+
              |D|
+-+-+   +-+   +-+
|B B|   |C|
+   +   + +-+
|B B|   |C C|
+-+-+   +-+ +
          |C|
+-+-+-+   +-+
|E E E|
+-+-+-+

Plants of the same type can appear in multiple separate regions, and regions can
even appear within other regions. For example:

OOOOO
OXOXO
OOOOO
OXOXO
OOOOO

The above map contains five regions, one containing all of the O garden plots,
and the other four each containing a single X plot.

The four X regions each have area 1 and perimeter 4. The region containing 21
type O plants is more complicated; in addition to its outer edge contributing a
perimeter of 20, its boundary with each X region contributes an additional 4 to
its perimeter, for a total perimeter of 36.

Due to "modern" business practices, the price of fence required for a region is
found by multiplying that region's area by its perimeter. The total price of
fencing all regions on a map is found by adding together the price of fence for
every region on the map.

In the first example, region A has price 4 * 10 = 40, region B has price
4 * 8 = 32, region C has price 4 * 10 = 40, region D has price 1 * 4 = 4, and
region E has price 3 * 8 = 24. So, the total price for the first example is 140.

In the second example, the region with all of the O plants has price
21 * 36 = 756, and each of the four smaller X regions has price 1 * 4 = 4, for
a total price of 772 (756 + 4 + 4 + 4 + 4).

Here's a larger example:

RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE

It contains:

- A region of R plants with price 12 * 18 = 216.
- A region of I plants with price 4 * 8 = 32.
- A region of C plants with price 14 * 28 = 392.
- A region of F plants with price 10 * 18 = 180.
- A region of V plants with price 13 * 20 = 260.
- A region of J plants with price 11 * 20 = 220.
- A region of C plants with price 1 * 4 = 4.
- A region of E plants with price 13 * 18 = 234.
- A region of I plants with price 14 * 22 = 308.
- A region of M plants with price 5 * 12 = 60.
- A region of S plants with price 3 * 8 = 24.
-
So, it has a total price of 1930.

What is the total price of fencing all regions on your map?

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
            S.filter (\p' -> S.notMember p' store && equals c p' g)
                . S.fromList
                . concatMap directNeighbors
                . S.toList
                $ frontier

{- | Part 2

Fortunately, the Elves are trying to order so much fence that they qualify for a
bulk discount!

Under the bulk discount, instead of using the perimeter to calculate the price,
you need to use the number of sides each region has. Each straight section of
fence counts as a side, regardless of how long it is.

Consider this example again:

AAAA
BBCD
BBCC
EEEC

The region containing type A plants has 4 sides, as does each of the regions
containing plants of type B, D, and E. However, the more complex region
containing the plants of type C has 8 sides!

Using the new method of calculating the per-region price by multiplying the
region's area by its number of sides, regions A through E have prices 16, 16,
32, 4, and 12, respectively, for a total price of 80.

The second example above (full of type X and O plants) would have a total price
of 436.

Here's a map that includes an E-shaped region full of type E plants:

EEEEE
EXXXX
EEEEE
EXXXX
EEEEE

The E-shaped region has an area of 17 and 12 sides for a price of 204. Including
the two regions full of type X plants, this map has a total price of 236.

This map has a total price of 368:

AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA

It includes two regions full of type B plants (each with 4 sides) and a single
region full of type A plants (with 4 sides on the outside and 8 more sides on
the inside, a total of 12 sides). Be especially careful when counting the fence
around regions like the one full of type A plants; in particular, each section
of fence has an in-side and an out-side, so the fence does not connect across
the middle of the region (where the two B regions touch diagonally). (The Elves
would have used the Möbius Fencing Company instead, but their contract terms
were too one-sided.)

The larger example from before now has the following updated prices:

- A region of R plants with price 12 * 10 = 120.
- A region of I plants with price 4 * 4 = 16.
- A region of C plants with price 14 * 22 = 308.
- A region of F plants with price 10 * 12 = 120.
- A region of V plants with price 13 * 10 = 130.
- A region of J plants with price 11 * 12 = 132.
- A region of C plants with price 1 * 4 = 4.
- A region of E plants with price 13 * 8 = 104.
- A region of I plants with price 14 * 16 = 224.
- A region of M plants with price 5 * 6 = 30.
- A region of S plants with price 3 * 6 = 18.

Adding these together produces its new total price of 1206.

What is the new total price of fencing all regions on your map?

Example should be 1206:

>>> let example = "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"
>>> part2 example
1206
-}
part2 :: String -> Int
part2 =
    sum . map (\gp -> area gp * countSides gp) . extractPlots . Grid2D . lines

data Direction = U | D | L | R deriving (Show, Eq, Ord)

data Side = Side Position Direction deriving (Show, Eq, Ord)

-- >>> countSides (GardenPlot (Position 9 3) (S.fromList [Position 7 2,Position 7 3,Position 8 0,Position 8 2,Position 8 3,Position 8 4,Position 9 0,Position 9 1,Position 9 2,Position 9 3]) 'F')
-- 12
countSides :: GardenPlot -> Int
countSides gp = nu + nd + nl + nr
  where
    allS = allPositionSides gp
    nu = length . groupSide U $ allS
    nd = length . groupSide D $ allS
    nl = length . groupSide L $ allS
    nr = length . groupSide R $ allS

-- >>> groupSide D [Side (Position 7 2) D,Side (Position 7 2) L,Side (Position 7 3) U,Side (Position 7 3) L,Side (Position 8 0) U,Side (Position 8 0) D,Side (Position 8 0) L,Side (Position 8 2) D,Side (Position 8 4) U,Side (Position 8 4) L,Side (Position 8 4) R,Side (Position 9 0) D,Side (Position 9 0) R,Side (Position 9 1) L,Side (Position 9 1) R,Side (Position 9 2) R,Side (Position 9 3) U,Side (Position 9 3) R]
-- [[Side (Position 8 0) D,Side (Position 9 0) D],[Side (Position 7 2) D,Side (Position 8 2) D]]
groupSide :: Direction -> [Side] -> [[Side]]
groupSide _ [] = []
groupSide U ss = flip concatSides [[]] . sortOn takeY . filter (isDirection U) $ ss
groupSide D ss = flip concatSides [[]] . sortOn takeY . filter (isDirection D) $ ss
groupSide L ss = flip concatSides [[]] . sortOn takeX . filter (isDirection L) $ ss
groupSide R ss = flip concatSides [[]] . sortOn takeX . filter (isDirection R) $ ss

concatSides :: [Side] -> [[Side]] -> [[Side]]
concatSides [] res = res
concatSides (s : rs) [[]] = concatSides rs [[s]]
concatSides (s : rs) res =
    if last (last res) `isNeighbor` s
        then concatSides rs (dropLast 1 res ++ [last res ++ [s]])
        else concatSides rs (res ++ [[s]])

takeY :: Side -> Int
takeY (Side (Position _ y) _) = y

takeX :: Side -> Int
takeX (Side (Position x _) _) = x

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
-- >>> allPositionSides (GardenPlot (Position 0 0) (S.fromList [Position 0 0,Position 0 1,Position 1 0,Position 1 1]) 'R')
-- >>> allPositionSides (GardenPlot (Position 9 3) (S.fromList [Position 7 2,Position 7 3,Position 8 0,Position 8 2,Position 8 3,Position 8 4,Position 9 0,Position 9 1,Position 9 2,Position 9 3]) 'F')
-- 18
-- [Side (Position 0 0) D,Side (Position 0 0) L,Side (Position 0 1) U,Side (Position 0 1) L,Side (Position 1 0) D,Side (Position 1 0) R,Side (Position 1 1) U,Side (Position 1 1) R]
-- [Side (Position 7 2) D,Side (Position 7 2) L,Side (Position 7 3) U,Side (Position 7 3) L,Side (Position 8 0) U,Side (Position 8 0) D,Side (Position 8 0) L,Side (Position 8 2) D,Side (Position 8 4) U,Side (Position 8 4) L,Side (Position 8 4) R,Side (Position 9 0) D,Side (Position 9 0) R,Side (Position 9 1) L,Side (Position 9 1) R,Side (Position 9 2) R,Side (Position 9 3) U,Side (Position 9 3) R]
allPositionSides :: GardenPlot -> [Side]
allPositionSides (GardenPlot _ ps _) = concatMap pSides ps
  where
    pSides p = filter (\(Side sp d) -> next sp d `notElem` ps) $ positionSides p

next :: Position -> Direction -> Position
next (Position x y) U = Position x (y + 1)
next (Position x y) R = Position (x + 1) y
next (Position x y) D = Position x (y - 1)
next (Position x y) L = Position (x - 1) y

positionSides :: Position -> [Side]
positionSides p = [Side p U, Side p D, Side p L, Side p R]

solve :: IO ()
solve = do
    content <- readFile "./src/Year2024/data/day12.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
