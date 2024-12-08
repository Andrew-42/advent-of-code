{-# LANGUAGE TupleSections #-}

module Year2024.Day08 (solve) where

import qualified Data.Set as S
import qualified Grid as G

{- | Part 1

You find yourselves on the roof of a top-secret Easter Bunny installation.

While The Historians do their thing, you take a look at the familiar huge
antenna. Much to your surprise, it seems to have been reconfigured to emit a
signal that makes people 0.1% more likely to buy Easter Bunny brand Imitation
Mediocre Chocolate as a Christmas gift! Unthinkable!

Scanning across the city, you find that there are actually many such antennas.
Each antenna is tuned to a specific frequency indicated by a single lowercase
letter, uppercase letter, or digit. You create a map (your puzzle input) of
these antennas. For example:

............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............

The signal only applies its nefarious effect at specific antinodes based on the
resonant frequencies of the antennas. In particular, an antinode occurs at any
point that is perfectly in line with two antennas of the same frequency - but
only when one of the antennas is twice as far away as the other. This means that
for any pair of antennas with the same frequency, there are two antinodes, one
    on either side of them.

So, for these two antennas with frequency a, they create the two antinodes
marked with #:

..........
...#......
..........
....a.....
..........
.....a....
..........
......#...
..........
..........

Adding a third antenna with the same frequency creates several more antinodes.
It would ideally add four antinodes, but two are off the right side of the map,
so instead it adds only two:

..........
...#......
#.........
....a.....
........a.
.....a....
..#.......
......#...
..........
..........

Antennas with different frequencies don't create antinodes; A and a count as
different frequencies. However, antinodes can occur at locations that contain
antennas. In this diagram, the lone antenna with frequency capital A creates no
antinodes but has a lowercase-a-frequency antinode at its location:

..........
...#......
#.........
....a.....
........a.
.....a....
..#.......
......A...
..........
..........

The first example has antennas with two different frequencies, so the antinodes
they create look like this, plus an antinode overlapping the topmost A-frequency
antenna:

......#....#
...#....0...
....#0....#.
..#....0....
....0....#..
.#....A.....
...#........
#......#....
........A...
.........A..
..........#.
..........#.

Because the topmost A-frequency antenna overlaps with a 0-frequency antinode,
there are 14 total unique locations that contain an antinode within the bounds
of the map.

Calculate the impact of the signal. How many unique locations within the bounds
of the map contain an antinode?

Example should be 14:

>>> let example = "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............"
>>> part1 example
14
-}
part1 :: String -> Int
part1 s =
    S.size
        . S.fromList
        . concatMap
            ( filter (`isInGrid` grid)
                . (concatMap (uncurry antinodes) . pairs)
                . (`positionsOf` grid)
            )
        $ anthenaChars
  where
    grid = G.Grid2D . lines $ s

anthenaChars :: String
anthenaChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

positionsOf :: Char -> G.Grid2D Char -> [G.Position]
positionsOf c g =
    [ G.Position x y
    | x <- [0 .. (dimX - 1)]
    , y <- [0 .. (dimY - 1)]
    , G.gridAt (G.Position x y) g == Just c
    ]
  where
    (dimX, dimY) = G.shape g

-- >>> pairs [G.Position 4 4,G.Position 5 2,G.Position 7 3]
-- [(Position 4 4,Position 5 2),(Position 4 4,Position 7 3),(Position 5 2,Position 7 3)]
pairs :: [G.Position] -> [(G.Position, G.Position)]
pairs [] = []
pairs [_] = []
pairs (p : rest) = map (p,) rest ++ pairs rest

-- >>> antinodes (G.Position 4 3) (G.Position 5 5)
-- [Position 3 1,Position 6 7]
antinodes :: G.Position -> G.Position -> [G.Position]
antinodes (G.Position x1 y1) (G.Position x2 y2) =
    [G.Position (x1 + dx) (y1 + dy), G.Position (x2 - dx) (y2 - dy)]
  where
    dx = x1 - x2
    dy = y1 - y2

isInGrid :: G.Position -> G.Grid2D a -> Bool
isInGrid (G.Position x y) g = (0 <= x && x < dimX) && (0 <= y && y < dimY)
  where
    (dimX, dimY) = G.shape g

isInDims :: G.Position -> (Int, Int) -> Bool
isInDims (G.Position x y) (dimX, dimY) =
    (0 <= x && x < dimX) && (0 <= y && y < dimY)

{- | Part 2

Watching over your shoulder as you work, one of The Historians asks if you took
the effects of resonant harmonics into your calculations.

Whoops!

After updating your model, it turns out that an antinode occurs at any grid
position exactly in line with at least two antennas of the same frequency,
regardless of distance. This means that some of the new antinodes will occur at
the position of each antenna (unless that antenna is the only one of its
frequency).

So, these three T-frequency antennas now create many antinodes:

T....#....
...T......
.T....#...
.........#
..#.......
..........
...#......
..........
....#.....
..........

In fact, the three T-frequency antennas are all exactly in line with two
antennas, so they are all also antinodes! This brings the total number of
antinodes in the above example to 9.

The original example now has 34 antinodes, including the antinodes that appear
on every antenna:

##....#....#
.#.#....0...
..#.#0....#.
..##...0....
....0....#..
.#...#A....#
...#..#.....
#....#.#....
..#.....A...
....#....A..
.#........#.
...#......##

Calculate the impact of the signal using this updated model. How many unique
locations within the bounds of the map contain an antinode?

Example should be 34:

>>> let example = "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............"
>>> part2 example
34
-}
part2 :: String -> Int
part2 s =
    S.size
        . S.fromList
        . concatMap
            ( (concatMap (uncurry (harmonicAntinodes shape)) . pairs)
                . (`positionsOf` grid)
            )
        $ anthenaChars
  where
    grid = G.Grid2D . lines $ s
    shape = G.shape grid

-- >>> harmonicAntinodes (12, 12) (G.Position 5 2) (G.Position 7 3)
-- [Position 7 3,Position 9 4,Position 11 5,Position 5 2,Position 3 1,Position 1 0]
harmonicAntinodes :: (Int, Int) -> G.Position -> G.Position -> [G.Position]
harmonicAntinodes (dimX, dimY) (G.Position x1 y1) (G.Position x2 y2) =
    positionsMinus 0 ++ positionsPlus 0
  where
    dx = x1 - x2
    dy = y1 - y2

    positionsPlus :: Int -> [G.Position]
    positionsPlus n =
        if isInDims newPosition (dimX, dimY)
            then newPosition : positionsPlus (n + 1)
            else []
      where
        newPosition = G.Position (x1 + n * dx) (y1 + n * dy)

    positionsMinus :: Int -> [G.Position]
    positionsMinus n =
        if isInDims newPosition (dimX, dimY)
            then newPosition : positionsMinus (n + 1)
            else []
      where
        newPosition = G.Position (x2 - n * dx) (y2 - n * dy)

solve :: IO ()
solve = do
    content <- readFile "./src/Year2024/data/day08.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
