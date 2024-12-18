module Year2024.Day18 (solve) where

import Control.Monad (foldM)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Grid (
    Grid2D,
    Position (Position),
    Shape (Shape),
    compareG,
    directNeighbors,
    mkGrid,
    setAllGridAt,
    setGridAt,
 )
import Search (SearchProblem (SearchProblem), bfs, safeBfs)
import Utils (itemAt)

{- | Part 1

You and The Historians look a lot more pixelated than you remember. You're
inside a computer at the North Pole!

Just as you're about to check out your surroundings, a program runs up to you.
"This region of memory isn't safe! The User misunderstood what a pushdown
automaton is and their algorithm is pushing whole bytes down on top of us! Run!"

The algorithm is fast - it's going to cause a byte to fall into your memory
space once every nanosecond! Fortunately, you're faster, and by quickly scanning
the algorithm, you create a list of which bytes will fall (your puzzle input) in
the order they'll land in your memory space.

Your memory space is a two-dimensional grid with coordinates that range from 0
to 70 both horizontally and vertically. However, for the sake of example,
suppose you're on a smaller grid with coordinates that range from 0 to 6 and the
following list of incoming byte positions:

5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0

Each byte position is given as an X,Y coordinate, where X is the distance from
the left edge of your memory space and Y is the distance from the top edge of
your memory space.

You and The Historians are currently in the top left corner of the memory space
(at 0,0) and need to reach the exit in the bottom right corner (at 70,70 in your
memory space, but at 6,6 in this example). You'll need to simulate the falling
bytes to plan out where it will be safe to run; for now, simulate just the first
few bytes falling into your memory space.

As bytes fall into your memory space, they make that coordinate corrupted.
Corrupted memory coordinates cannot be entered by you or The Historians, so
you'll need to plan your route carefully. You also cannot leave the boundaries
of the memory space; your only hope is to reach the exit.

In the above example, if you were to draw the memory space after the first 12
bytes have fallen (using . for safe and # for corrupted), it would look like
this:

...#...
..#..#.
....#..
...#..#
..#..#.
.#..#..
#.#....

You can take steps up, down, left, or right. After just 12 bytes have corrupted
locations in your memory space, the shortest path from the top left corner to
the exit would take 22 steps. Here (marked with O) is one such path:

OO.#OOO
.O#OO#O
.OOO#OO
...#OO#
..#OO#.
.#.O#..
#.#OOOO

Simulate the first kilobyte (1024 bytes) falling onto your memory space.
Afterward, what is the minimum number of steps needed to reach the exit?

Example:

>>> let example = "5,4\n4,2\n4,5\n3,0\n2,1\n6,3\n2,4\n1,5\n0,6\n3,3\n2,6\n5,1\n1,2\n5,5\n2,5\n6,5\n1,4\n0,4\n6,4\n1,1\n6,1\n1,0\n0,5\n1,6\n2,0"
>>> part1 example 12 7
Just (Solution 22 (Position 6 6) [Position 6 6,Position 5 6,Position 4 6,Position 3 6,Position 3 5,Position 3 4,Position 4 4,Position 4 3,Position 5 3,Position 5 2,Position 6 2,Position 6 1,Position 6 0,Position 5 0,Position 4 0,Position 4 1,Position 3 1,Position 3 2,Position 2 2,Position 1 2,Position 0 2,Position 0 1,Position 0 0])
-}
part1 :: String -> Int -> Int -> Maybe Solution
part1 s mSize dim = do
    g <- setAllGridAt '#' ps emptyG
    let (start, end) = (Position 0 0, Position (dim - 1) (dim - 1))
    let sol = Solution 0 start [start]
    let sp = SearchProblem sol scoreG (advance g) (isDone end)
    return $ bfs sp
  where
    ps = take mSize $ pInput s
    emptyG = mkGrid '.' (Shape dim dim)
    scoreG = mkGrid 0 (Shape dim dim)

data Solution = Solution Int Position [Position] deriving (Show, Eq, Ord)

advance :: Grid2D Char -> Solution -> Grid2D Int -> (S.Set Solution, Grid2D Int)
advance g (Solution score p ps) sg = case maybeNsg of
    Just nsg -> (S.fromList ss, nsg)
    Nothing -> (S.empty, sg)
  where
    maybeNsg = foldM (\sg' (Solution sc p' _) -> setGridAt sc p' sg') sg ss
    ss =
        map (\p' -> Solution ns p' (p' : ps))
            . filter (\p' -> compareG (\x -> x == 0 || x > ns) p' sg)
            . filter (\p' -> compareG (/= '#') p' g)
            . directNeighbors
            $ p
    ns = score + 1

isDone :: Position -> Solution -> Bool
isDone end (Solution _ p _) = end == p

{- | Part 2

The Historians aren't as used to moving around in this pixelated universe as you
are. You're afraid they're not going to be fast enough to make it to the exit
before the path is completely blocked.

To determine how fast everyone needs to go, you need to determine the first byte
that will cut off the path to the exit.

In the above example, after the byte at 1,1 falls, there is still a path to the
exit:

O..#OOO
O##OO#O
O#OO#OO
OOO#OO#
###OO##
.##O###
#.#OOOO

However, after adding the very next byte (at 6,1), there is no longer a path to
the exit:

...#...
.##..##
.#..#..
...#..#
###..##
.##.###
#.#....

So, in this example, the coordinates of the first byte that prevents the exit
from being reachable are 6,1.

Simulate more of the bytes that are about to corrupt your memory space. What are
the coordinates of the first byte that will prevent the exit from being
reachable from your starting position? (Provide the answer as two integers
separated by a comma with no other characters.)

Example:

>>> let example = "5,4\n4,2\n4,5\n3,0\n2,1\n6,3\n2,4\n1,5\n0,6\n3,3\n2,6\n5,1\n1,2\n5,5\n2,5\n6,5\n1,4\n0,4\n6,4\n1,1\n6,1\n1,0\n0,5\n1,6\n2,0"
>>> part2 example 12 7
Just (20,Position 6 1)
-}
part2 :: String -> Int -> Int -> Maybe (Int, Position)
part2 s mSize dim = do
    n <- unsolvable mSize
    p <- n `itemAt` ps
    return (n, p)
  where
    ps = pInput s
    emptyG = mkGrid '.' (Shape dim dim)
    scoreG = mkGrid 0 (Shape dim dim)

    unsolvable :: Int -> Maybe Int
    unsolvable n =
        if n > length ps
            then Nothing
            else do
                g <- setAllGridAt '#' (take n ps) emptyG
                let (start, end) = (Position 0 0, Position (dim - 1) (dim - 1))
                let sol = Solution 0 start [start]
                let sp = SearchProblem sol scoreG (advance g) (isDone end)
                case safeBfs sp of
                    Just _ -> unsolvable (n + 1)
                    Nothing -> Just (n - 1)

pInput :: String -> [Position]
pInput =
    map ((Position <$> (read . head) <*> (read . last)) . splitOn ",") . lines

solve :: IO ()
solve = do
    -- example <- readFile "./src/Year2024/data/day18-example.txt"
    -- print $ "Part1 example: " ++ show (part1 example)
    -- print $ "Part2 example: " ++ show (part2 example)

    content <- readFile "./src/Year2024/data/day18.txt"
    print $ "Part1 solution: " ++ show (part1 content 1024 71)
    print $ "Part2 solution: " ++ show (part2 content 1024 71)
