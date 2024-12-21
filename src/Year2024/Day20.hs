module Year2024.Day20 (solve) where

import Data.Maybe (mapMaybe)
import Grid (
    Grid2D (Grid2D),
    Position (Position),
    directNeighbors,
    gridAt,
    manhattanDistance,
    positionOf,
    setGridAt,
 )
import Utils (indexed, indexed1)

{- | Part 1

The Historians are quite pixelated again. This time, a massive, black building
looms over you - you're right outside the CPU!

While The Historians get to work, a nearby program sees that you're idle and
challenges you to a race. Apparently, you've arrived just in time for the
frequently-held race condition festival!

The race takes place on a particularly long and twisting code path; programs
compete to see who can finish in the fewest picoseconds. The winner even gets
their very own mutex!

They hand you a map of the racetrack (your puzzle input). For example:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

The map consists of track (.) - including the start (S) and end (E) positions
(both of which also count as track) - and walls (#).

When a program runs through the racetrack, it starts at the start position.
Then, it is allowed to move up, down, left, or right; each such move takes 1
picosecond. The goal is to reach the end position as quickly as possible. In
this example racetrack, the fastest time is 84 picoseconds.

Because there is only a single path from the start to the end and the programs
all go the same speed, the races used to be pretty boring. To make things more
interesting, they introduced a new rule to the races: programs are allowed to
cheat.

The rules for cheating are very strict. Exactly once during a race, a program
may disable collision for up to 2 picoseconds. This allows the program to pass
through walls as if they were regular track. At the end of the cheat, the
program must be back on normal track again; otherwise, it will receive a
segmentation fault and get disqualified.

So, a program could complete the course in 72 picoseconds (saving 12
picoseconds) by cheating for the two moves marked 1 and 2:

###############
#...#...12....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

Or, a program could complete the course in 64 picoseconds (saving 20
picoseconds) by cheating for the two moves marked 1 and 2:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...12..#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

This cheat saves 38 picoseconds:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.####1##.###
#...###.2.#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

This cheat saves 64 picoseconds and takes the program directly to the end:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..21...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

Each cheat has a distinct start position (the position where the cheat is
activated, just before the first move that is allowed to go through walls) and
end position; cheats are uniquely identified by their start position and end
position.

In this example, the total number of cheats (grouped by the amount of time they
save) are as follows:

- There are 14 cheats that save 2 picoseconds.
- There are 14 cheats that save 4 picoseconds.
- There are 2 cheats that save 6 picoseconds.
- There are 4 cheats that save 8 picoseconds.
- There are 2 cheats that save 10 picoseconds.
- There are 3 cheats that save 12 picoseconds.
- There is one cheat that saves 20 picoseconds.
- There is one cheat that saves 36 picoseconds.
- There is one cheat that saves 38 picoseconds.
- There is one cheat that saves 40 picoseconds.
- There is one cheat that saves 64 picoseconds.
-
You aren't sure what the conditions of the racetrack will be like, so to give
yourself as many options as possible, you'll need a list of the best cheats. How
many cheats would save you at least 100 picoseconds?

Example should be 44:

>>> let example = "###############\n#...#...#.....#\n#.#.#.#.#.###.#\n#S#...#.#.#...#\n#######.#.#.###\n#######.#.#...#\n#######.#.###.#\n###..E#...#...#\n###.#######.###\n#...###...#...#\n#.#####.#.###.#\n#.#...#.#.#...#\n#.#.#.#.#.#.###\n#...#...#...###\n###############"
>>> part1 example 2
Just 44
-}
part1 :: String -> Int -> Maybe Int
part1 s minC = do
    (start, end, g) <- pInput s
    let path = reverse $ racePath g [start] (isDone end)
    let cheats = findCheats minC path []
    return $ length cheats

racePath :: Grid2D Char -> [Position] -> (Position -> Bool) -> [Position]
racePath _ [] _ = []
racePath g r@[h] f = if f np then np : r else racePath g (np : r) f
  where
    np = head . filter (`isDot` g) . directNeighbors $ h
racePath g r@(h1 : h2 : _) f = if f np then np : r else racePath g (np : r) f
  where
    np = head . filter (\p -> isDot p g && p /= h2) . directNeighbors $ h1

isDot :: Position -> Grid2D Char -> Bool
isDot p' g = case gridAt p' g of
    Just '.' -> True
    Just '#' -> False
    Just _ -> False
    Nothing -> False

isDone :: Position -> Position -> Bool
isDone pe p = pe == p

data Cheat = Cheat Int Position Position deriving (Show, Eq, Ord)

findCheats :: Int -> [Position] -> [Cheat] -> [Cheat]
findCheats _ [] chs = chs
findCheats minC (p : ps) chs = findCheats minC ps (nCheats ++ chs)
  where
    nCheats = mapMaybe (\(i, p') -> toCheat p p' i) $ drop (minC + 1) (indexed1 ps)

toCheat :: Position -> Position -> Int -> Maybe Cheat
toCheat p1 p2 n =
    if p2 `elem` cheatNeighbors p1
        then Just (Cheat (n - 2) p1 p2)
        else Nothing

cheatNeighbors :: Position -> [Position]
cheatNeighbors (Position x y) =
    [ Position (x - 2) y
    , Position (x + 2) y
    , Position x (y - 2)
    , Position x (y + 2)
    ]

{- | Part 2

The programs seem perplexed by your list of cheats. Apparently, the
two-picosecond cheating rule was deprecated several milliseconds ago! The latest
version of the cheating rule permits a single cheat that instead lasts at most
20 picoseconds.

Now, in addition to all the cheats that were possible in just two picoseconds,
many more cheats are possible. This six-picosecond cheat saves 76 picoseconds:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#1#####.#.#.###
#2#####.#.#...#
#3#####.#.###.#
#456.E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

Because this cheat has the same start and end positions as the one above, it's
the same cheat, even though the path taken during the cheat is different:

###############
#...#...#.....#
#.#.#.#.#.###.#
#S12..#.#.#...#
###3###.#.#.###
###4###.#.#...#
###5###.#.###.#
###6.E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############

Cheats don't need to use all 20 picoseconds; cheats can last any amount of time
up to and including 20 picoseconds (but can still only end when the program is
on normal track). Any cheat time not used is lost; it can't be saved for another
cheat later.

You'll still need a list of the best cheats, but now there are even more to
choose between. Here are the quantities of cheats in this example that save 50
picoseconds or more:

- There are 32 cheats that save 50 picoseconds.
- There are 31 cheats that save 52 picoseconds.
- There are 29 cheats that save 54 picoseconds.
- There are 39 cheats that save 56 picoseconds.
- There are 25 cheats that save 58 picoseconds.
- There are 23 cheats that save 60 picoseconds.
- There are 20 cheats that save 62 picoseconds.
- There are 19 cheats that save 64 picoseconds.
- There are 12 cheats that save 66 picoseconds.
- There are 14 cheats that save 68 picoseconds.
- There are 12 cheats that save 70 picoseconds.
- There are 22 cheats that save 72 picoseconds.
- There are 4 cheats that save 74 picoseconds.
- There are 3 cheats that save 76 picoseconds.

Find the best cheats using the updated cheating rules. How many cheats would
save you at least 100 picoseconds?

Example should be 285:

>>> let example = "###############\n#...#...#.....#\n#.#.#.#.#.###.#\n#S#...#.#.#...#\n#######.#.#.###\n#######.#.#...#\n#######.#.###.#\n###..E#...#...#\n###.#######.###\n#...###...#...#\n#.#####.#.###.#\n#.#...#.#.#...#\n#.#.#.#.#.#.###\n#...#...#...###\n###############"
>>> part2 example 50 20
Just 285
-}
part2 :: String -> Int -> Int -> Maybe Int
part2 s minSave maxDistance = do
    (start, end, g) <- pInput s
    let path = indexed . reverse $ racePath g [start] (isDone end)
    return $ countCheats (minSave, maxDistance) path 0

countCheats :: (Int, Int) -> [(Int, Position)] -> Int -> Int
countCheats _ [] chs = chs
countCheats c@(minSave, _) (np : ps) chs = countCheats c ps (chs + nc)
  where
    nc = length . filter (isCheatDestination c np) . drop minSave $ ps

isCheatDestination :: (Int, Int) -> (Int, Position) -> (Int, Position) -> Bool
isCheatDestination (minSave, maxDistance) (n1, p1) (n2, p2) =
    d <= maxDistance && (n2 - (n1 + d)) >= minSave
  where
    d = manhattanDistance p1 p2

-- >>> let example = "###############\n#...#...#.....#\n#.#.#.#.#.###.#\n#S#...#.#.#...#\n#######.#.#.###\n#######.#.#...#\n#######.#.###.#\n###..E#...#...#\n###.#######.###\n#...###...#...#\n#.#####.#.###.#\n#.#...#.#.#...#\n#.#.#.#.#.#.###\n#...#...#...###\n###############"
-- >>> pInput example
-- Just (Position 1 3,Position 5 7,Grid2D ["###############","#...#...#.....#","#.#.#.#.#.###.#","#.#...#.#.#...#","#######.#.#.###","#######.#.#...#","#######.#.###.#","###...#...#...#","###.#######.###","#...###...#...#","#.#####.#.###.#","#.#...#.#.#...#","#.#.#.#.#.#.###","#...#...#...###","###############"])
pInput :: String -> Maybe (Position, Position, Grid2D Char)
pInput s = do
    let g = Grid2D . lines $ s
    start <- positionOf 'S' g
    end <- positionOf 'E' g
    g1 <- setGridAt '.' start g
    g2 <- setGridAt '.' end g1
    return (start, end, g2)

solve :: IO ()
solve = do
    content <- readFile "./src/Year2024/data/day20.txt"
    print $ "Part1 solution: " ++ show (part1 content 100)
    print $ "Part2 solution: " ++ show (part2 content 100 20)
