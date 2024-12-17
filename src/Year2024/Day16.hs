module Year2024.Day16 (solve) where

import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Grid (
    Direction (East),
    Grid2D (Grid2D),
    Position,
    gridAt,
    next,
    positionOf,
    setGridAt,
    shape,
    turnLeft,
    turnRight,
    zeros,
 )

{- | Part 1

It's time again for the Reindeer Olympics! This year, the big event is the
Reindeer Maze, where the Reindeer compete for the lowest score.

You and The Historians arrive to search for the Chief right as the event is
about to start. It wouldn't hurt to watch a little, right?

The Reindeer start on the Start Tile (marked S) facing East and need to reach
the End Tile (marked E). They can move forward one tile at a time (increasing
their score by 1 point), but never into a wall (#). They can also rotate
clockwise or counterclockwise 90 degrees at a time (increasing their score by
1000 points).

To figure out the best place to sit, you start by grabbing a map (your puzzle
input) from a nearby kiosk. For example:

###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############

There are many paths through this maze, but taking any of the best paths would
incur a score of only 7036. This can be achieved by taking a total of 36 steps
forward and turning 90 degrees a total of 7 times:

###############
#.......#....E#
#.#.###.#.###^#
#.....#.#...#^#
#.###.#####.#^#
#.#.#.......#^#
#.#.#####.###^#
#..>>>>>>>>v#^#
###^#.#####v#^#
#>>^#.....#v#^#
#^#.#.###.#v#^#
#^....#...#v#^#
#^###.#.#.#v#^#
#S..#.....#>>^#
###############

Here's a second example:

#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################

In this maze, the best paths cost 11048 points; following one such path would
look like this:

#################
#...#...#...#..E#
#.#.#.#.#.#.#.#^#
#.#.#.#...#...#^#
#.#.#.#.###.#.#^#
#>>v#.#.#.....#^#
#^#v#.#.#.#####^#
#^#v..#.#.#>>>>^#
#^#v#####.#^###.#
#^#v#..>>>>^#...#
#^#v###^#####.###
#^#v#>>^#.....#.#
#^#v#^#####.###.#
#^#v#^........#.#
#^#v#^#########.#
#S#>>^..........#
#################

Note that the path shown above includes one 90 degree turn as the very first
move, rotating the Reindeer from facing East to facing North.

Analyze your map carefully. What is the lowest score a Reindeer could possibly
get?

Example should be 7036:

>>> let example = "###############\n#.......#....E#\n#.#.###.#.###.#\n#.....#.#...#.#\n#.###.#####.#.#\n#.#.#.......#.#\n#.#.#####.###.#\n#...........#.#\n###.#.#####.#.#\n#...#.....#.#.#\n#.#.#.###.#.#.#\n#.....#...#.#.#\n#.###.#.#.#.#.#\n#S..#.....#...#\n###############"
>>> part1 example
Just 7036
-}
part1 :: String -> Maybe Int
part1 s = do
    (r, e, g) <- pInput s
    let sp = SearchProblem r g advance (isDone e)
    let (Reindeer score _ _ _) = bfs sp
    return score

data Reindeer = Reindeer Int Position Direction [Position]
    deriving (Show, Eq, Ord)

data SearchProblem a b = SearchProblem a b (a -> b -> (S.Set a, b)) (a -> Bool)

bfs :: SearchProblem Reindeer (Grid2D Char) -> Reindeer
bfs (SearchProblem start g step finished) = loop (S.singleton start) g
  where
    loop :: S.Set Reindeer -> Grid2D Char -> Reindeer
    loop xs g'
        | finished h = h
        | otherwise = loop (S.union rest nr) ng
      where
        (nr, ng) = step h g'
        (h, rest) = pop xs

pop :: S.Set a -> (a, S.Set a)
pop xs = (S.elemAt 0 xs, S.deleteAt 0 xs)

advance :: Reindeer -> Grid2D Char -> (S.Set Reindeer, Grid2D Char)
advance r@(Reindeer s rp d ps) g = case gridAt np g of
    Just '#' -> (S.fromList rRs, force (setGridAt 'O' rp g))
    Just '.' ->
        (S.fromList $ Reindeer (s + 1) np d (np : ps) : rRs, force (setGridAt 'O' np g))
    Just _ -> (S.empty, g)
    Nothing -> (S.empty, g)
  where
    np = next rp d
    rRs = rotate r g turnLeft ++ rotate r g turnRight

force :: Maybe a -> a
force (Just x) = x
force Nothing = error "The value must be just."

rotate :: Reindeer -> Grid2D Char -> (Direction -> Direction) -> [Reindeer]
rotate (Reindeer s rp d ps) g rt =
    if np `elem` ps
        then []
        else case gridAt np g of
            Just '.' -> [Reindeer (s + 1000) rp (rt d) ps]
            Just _ -> []
            Nothing -> []
  where
    np = next rp (rt d)

isDone :: Position -> Reindeer -> Bool
isDone p (Reindeer _ rp _ _) = p == rp

{- | Part 2

Now that you know what the best paths look like, you can figure out the best
spot to sit.

Every non-wall tile (S, ., or E) is equipped with places to sit along the edges
of the tile. While determining which of these tiles would be the best spot to
sit depends on a whole bunch of factors (how comfortable the seats are, how far
away the bathrooms are, whether there's a pillar blocking your view, etc.), the
most important factor is whether the tile is on one of the best paths through
the maze. If you sit somewhere else, you'd miss all the action!

So, you'll need to determine which tiles are part of any best path through the
maze, including the S and E tiles.

In the first example, there are 45 tiles (marked O) that are part of at least
one of the various best paths through the maze:

###############
#.......#....O#
#.#.###.#.###O#
#.....#.#...#O#
#.###.#####.#O#
#.#.#.......#O#
#.#.#####.###O#
#..OOOOOOOOO#O#
###O#O#####O#O#
#OOO#O....#O#O#
#O#O#O###.#O#O#
#OOOOO#...#O#O#
#O###.#.#.#O#O#
#O..#.....#OOO#
###############

In the second example, there are 64 tiles that are part of at least one of the
best paths:

#################
#...#...#...#..O#
#.#.#.#.#.#.#.#O#
#.#.#.#...#...#O#
#.#.#.#.###.#.#O#
#OOO#.#.#.....#O#
#O#O#.#.#.#####O#
#O#O..#.#.#OOOOO#
#O#O#####.#O###O#
#O#O#..OOOOO#OOO#
#O#O###O#####O###
#O#O#OOO#..OOO#.#
#O#O#O#####O###.#
#O#O#OOOOOOO..#.#
#O#O#O#########.#
#O#OOO..........#
#################

Analyze your map further. How many tiles are part of at least one of the best
paths through the maze?

Example should be 45:

>>> let example = "###############\n#.......#....E#\n#.#.###.#.###.#\n#.....#.#...#.#\n#.###.#####.#.#\n#.#.#.......#.#\n#.#.#####.###.#\n#...........#.#\n###.#.#####.#.#\n#...#.....#.#.#\n#.#.#.###.#.#.#\n#.....#...#.#.#\n#.###.#.#.#.#.#\n#S..#.....#...#\n###############"
>>> part2 example 7036
Just 45
-}
part2 :: String -> Int -> Maybe Int
part2 s score = do
    (r, e, g) <- pInput s
    let sg = zeros (shape g)
    let sp = SearchProblemM r sg (advance' g) (isDone e) (validScore score)
    let rs = bfsMax sp
    return $ S.size . S.fromList . concatMap takePositions $ rs

data SearchProblemM a b
    = SearchProblemM a b (a -> b -> ([a], b)) (a -> Bool) (a -> Bool)

bfsMax :: SearchProblemM Reindeer (Grid2D Int) -> [Reindeer]
bfsMax (SearchProblemM start sg step finished endF) = loop [] (S.singleton start) sg
  where
    loop :: [Reindeer] -> S.Set Reindeer -> Grid2D Int -> [Reindeer]
    loop res xs sg'
        | S.null xs = res
        | finished h = loop (h : res) rest nsg
        | otherwise = loop res (S.union rest (S.fromList $ filter endF nr)) nsg
      where
        (h, rest) = pop xs
        (nr, nsg) = step h sg'

validScore :: Int -> Reindeer -> Bool
validScore m (Reindeer s _ _ _) = s <= m

advance' :: Grid2D Char -> Reindeer -> Grid2D Int -> ([Reindeer], Grid2D Int)
advance' g r@(Reindeer s rp d ps) sg = case gridAt np g of
    Just '#' -> (rRs, sg)
    Just '.' -> case gridAt np sg of
        Just 0 -> (nr, fromMaybe sg (setGridAt (s + 1) np sg))
        -- Allow a turn when discarding multiple visits to one node
        Just x -> if (x + 1500) <= s + 1 then ([], sg) else (nr, sg)
        Nothing -> ([], sg)
      where
        nr = Reindeer (s + 1) np d (np : ps) : rRs
    Just _ -> ([], sg)
    Nothing -> ([], sg)
  where
    np = next rp d
    rRs = rotate r g turnLeft ++ rotate r g turnRight

takePositions :: Reindeer -> [Position]
takePositions (Reindeer _ _ _ ps) = ps

pInput :: String -> Maybe (Reindeer, Position, Grid2D Char)
pInput s = do
    let g = Grid2D . lines $ s
    rp <- positionOf 'S' g
    g1 <- setGridAt '.' rp g
    ep <- positionOf 'E' g1
    g2 <- setGridAt '.' ep g1
    return (Reindeer 0 rp East [rp], ep, g2)

solve :: IO ()
solve = do
    example <- readFile "./src/Year2024/data/day16-test.txt"
    print $ "Part1 example: " ++ show (part1 example)
    print $ "Part2 example: " ++ show (part2 example 11048)

    content <- readFile "./src/Year2024/data/day16.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content 90460)
