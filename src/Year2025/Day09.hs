module Year2025.Day09 (solve) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Utils (pairs, zipWithNext)

{- | --- Day 9: Movie Theater ---

You slide down the firepole in the corner of the playground and land in the North Pole base movie
theater!

The movie theater has a big tile floor with an interesting pattern. Elves here are redecorating the
theater by switching out some of the square tiles in the big grid they form. Some of the tiles are
red; the Elves would like to find the largest rectangle that uses red tiles for two of its opposite
corners. They even have a list of where the red tiles are located in the grid (your puzzle input).

For example:

7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3

Showing red tiles as # and other tiles as ., the above arrangement of red tiles would look like
this:

..............
.......#...#..
..............
..#....#......
..............
..#......#....
..............
.........#.#..
..............

You can choose any two red tiles as the opposite corners of your rectangle; your goal is to find the
largest rectangle possible.

For example, you could make a rectangle (shown as O) with an area of 24 between 2,5 and 9,7:

..............
.......#...#..
..............
..#....#......
..............
..OOOOOOOO....
..OOOOOOOO....
..OOOOOOOO.#..
..............

Or, you could make a rectangle with area 35 between 7,1 and 11,7:

..............
.......OOOOO..
.......OOOOO..
..#....OOOOO..
.......OOOOO..
..#....OOOOO..
.......OOOOO..
.......OOOOO..
..............

You could even make a thin rectangle with an area of only 6 between 7,3 and 2,3:

..............
.......#...#..
..............
..OOOOOO......
..............
..#......#....
..............
.........#.#..
..............

Ultimately, the largest rectangle you can make in this example has area 50. One way to do this is
between 2,5 and 11,1:

..............
..OOOOOOOOOO..
..OOOOOOOOOO..
..OOOOOOOOOO..
..OOOOOOOOOO..
..OOOOOOOOOO..
..............
.........#.#..
..............

Using two red tiles as opposite corners, what is the largest area of any rectangle you can make?

Example:

>>> let example = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"
>>> part1 example
50
-}
part1 :: T.Text -> Int
part1 = maximum . map (areaRec . uncurry Rectangle) . pairs . pInput

{- | Part 2

The Elves just remembered: they can only switch out tiles that are red or green. So, your rectangle
can only include red or green tiles.

In your list, every red tile is connected to the red tile before and after it by a straight line of
green tiles. The list wraps, so the first red tile is also connected to the last red tile. Tiles
that are adjacent in your list will always be on either the same row or the same column.

Using the same example as before, the tiles marked X would be green:

..............
.......#XXX#..
.......X...X..
..#XXXX#...X..
..X........X..
..#XXXXXX#.X..
.........X.X..
.........#X#..
..............

In addition, all of the tiles inside this loop of red and green tiles are also green. So, in this
example, these are the green tiles:

..............
.......#XXX#..
.......XXXXX..
..#XXXX#XXXX..
..XXXXXXXXXX..
..#XXXXXX#XX..
.........XXX..
.........#X#..
..............

The remaining tiles are never red nor green.

The rectangle you choose still must have red tiles in opposite corners, but any other tiles it
includes must now be red or green. This significantly limits your options.

For example, you could make a rectangle out of red and green tiles with an area of 15 between 7,3
and 11,1:

..............
.......OOOOO..
.......OOOOO..
..#XXXXOOOOO..
..XXXXXXXXXX..
..#XXXXXX#XX..
.........XXX..
.........#X#..
..............

Or, you could make a thin rectangle with an area of 3 between 9,7 and 9,5:

..............
.......#XXX#..
.......XXXXX..
..#XXXX#XXXX..
..XXXXXXXXXX..
..#XXXXXXOXX..
.........OXX..
.........OX#..
..............

The largest rectangle you can make in this example using only red and green tiles has area 24. One
way to do this is between 9,5 and 2,3:

..............
.......#XXX#..
.......XXXXX..
..OOOOOOOOXX..
..OOOOOOOOXX..
..OOOOOOOOXX..
.........XXX..
.........#X#..
..............

Using two red tiles as opposite corners, what is the largest area of any rectangle you can make
using only red and green tiles?

Example:

>>> let example = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"
>>> part2 example
24
-}
part2 :: T.Text -> Int
part2 text = maximum . map areaRec . filter (recInside $ createPolygon points) $ recs
  where
    points = pInput text
    recs = map (uncurry Rectangle) . pairs $ points

pInput :: T.Text -> [Point]
pInput = map pPoint . T.lines
  where
    pPoint :: T.Text -> Point
    pPoint text = case T.splitOn "," text of
        [x, y] -> Point (readT x) (readT y)
        _ -> error $ "Invalid coordinates input: " ++ T.unpack text

    readT = read . T.unpack

data Point = Point {getX :: Int, getY :: Int} deriving (Eq, Show)

data Rectangle = Rectangle Point Point deriving (Show)

otherVertices :: Rectangle -> (Point, Point)
otherVertices (Rectangle p1 p2) = (Point p1.getX p2.getY, Point p2.getX p1.getY)

recEdges :: Rectangle -> [Edge]
recEdges rec@(Rectangle p1 p3) = getEdges $ createPolygon [p1, p2, p3, p4]
  where
    (p2, p4) = otherVertices rec

recInside :: Polygon -> Rectangle -> Bool
recInside polygon r = pointInside polygon p1 && pointInside polygon p2 && not crossed
  where
    (p1, p2) = otherVertices r
    crossed = any (\e -> any (`crosses` e) $ getEdges polygon) $ recEdges r

areaRec :: Rectangle -> Int
areaRec (Rectangle p1 p2) = (abs (p1.getX - p2.getX) + 1) * (abs (p1.getY - p2.getY) + 1)

data Edge = Edge {getStart :: Point, getEnd :: Point} deriving (Eq, Show)

isVertical :: Edge -> Bool
isVertical (Edge s e) = s.getX == e.getX

isHorizontal :: Edge -> Bool
isHorizontal (Edge s e) = s.getY == e.getY

crosses :: Edge -> Edge -> Bool
crosses edge1@(Edge s1 e1) edge2@(Edge s2 e2)
    | isVertical edge1 && isHorizontal edge2 =
        inInterval s2.getX e2.getX s1.getX && inInterval s1.getY e1.getY s2.getY
    | isHorizontal edge1 && isVertical edge2 =
        inInterval s1.getX e1.getX s2.getX && inInterval s2.getY e2.getY s1.getY
    | otherwise = False

isOnEdge :: Edge -> Point -> Bool
isOnEdge edge@(Edge s e) p
    | s.getX == e.getX = (p.getX == s.getX) && (inInterval s.getY e.getY p.getY || isSE)
    | s.getY == e.getY = (p.getY == s.getY) && (inInterval s.getX e.getX p.getX || isSE)
    | otherwise = error $ "Invalid edge: " ++ show edge
  where
    isSE = p == s || p == e

inInterval :: Int -> Int -> Int -> Bool
inInterval a b x = if a < b then a < x && x < b else b < x && x < a

rightCrosses :: Edge -> Point -> Bool
rightCrosses edge@(Edge s e) p =
    isVertical edge
        && p.getX < s.getX
        && (min s.getY e.getY <= p.getY && p.getY < max s.getY e.getY)

newtype Polygon = Polygon {getEdges :: [Edge]} deriving (Show)

createPolygon :: [Point] -> Polygon
createPolygon ps = Polygon $ Edge (last ps) (head ps) : map (uncurry Edge) (zipWithNext ps)

pointInside :: Polygon -> Point -> Bool
pointInside (Polygon edges) p =
    any (`isOnEdge` p) edges || (odd . length . filter (`rightCrosses` p) $ edges)

solve :: IO ()
solve = do
    content <- TIO.readFile "./src/Year2025/data/day09.txt"
    -- Part1 solution: 4759930955
    print $ "Part1 solution: " ++ show (part1 content)
    -- Part2 solution: 1525241870
    print $ "Part2 solution: " ++ show (part2 content)
