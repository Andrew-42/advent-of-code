module Year2024.Day06 (solve) where

import Data.List (elemIndex, findIndex)
import qualified Data.Set as S
import Utils (itemAt, replace, setItemAt)

{- | Part 1

The Historians use their fancy device again, this time to whisk you all away to
the North Pole prototype suit manufacturing lab... in the year 1518! It turns
out that having direct access to history is very convenient for a group of
historians.

You still have to be careful of time paradoxes, and so it will be important to
avoid anyone from 1518 while The Historians search for the Chief. Unfortunately,
a single guard is patrolling this part of the lab.

Maybe you can work out where the guard will go ahead of time so that The
Historians can search safely?

You start by making a map (your puzzle input) of the situation. For example:

....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...

The map shows the current position of the guard with ^ (to indicate the guard is
currently facing up from the perspective of the map). Any obstructions - crates,
desks, alchemical reactors, etc. - are shown as #.

Lab guards in 1518 follow a very strict patrol protocol which involves
repeatedly following these steps:

If there is something directly in front of you, turn right 90 degrees.
Otherwise, take a step forward.
Following the above protocol, the guard moves up several times until she reaches
an obstacle (in this case, a pile of failed suit prototypes):

....#.....
....^....#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#...

Because there is now an obstacle in front of the guard, she turns right before
continuing straight in her new facing direction:

....#.....
........>#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#...

Reaching another obstacle (a spool of several very long polymers), she turns
right again and continues downward:

....#.....
.........#
..........
..#.......
.......#..
..........
.#......v.
........#.
#.........
......#...

This process continues for a while, but the guard eventually leaves the mapped
area (after walking past a tank of universal solvent):

....#.....
.........#
..........
..#.......
.......#..
..........
.#........
........#.
#.........
......#v..
By predicting the guard's route, you can determine which specific positions in
the lab will be in the patrol path. Including the guard's starting position, the
positions visited by the guard before leaving the area are marked with an X:

....#.....
....XXXXX#
....X...X.
..#.X...X.
..XXXXX#X.
..X.X.X.X.
.#XXXXXXX.
.XXXXXXX#.
#XXXXXXX..
......#X..

In this example, the guard will visit 41 distinct positions on your map.

Predict the path of the guard. How many distinct positions will the guard visit
before leaving the mapped area?

Example should be 41:

>>> let example = "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."
>>> part1 example
41
-}
part1 :: String -> Int
part1 s = case getInitPosition i of
    Just p -> countPositions $ runGuard p m
    Nothing -> countPositions m
  where
    i = reverse . lines $ s
    m = Map $ map (replace '^' 'X') i

data Direction = L | T | R | D deriving (Show, Eq, Ord)

data Guard = Guard Position Direction deriving (Show, Eq, Ord)

data Position = Position Int Int deriving (Show, Eq, Ord)

newtype Map = Map {getMap :: [String]} deriving (Show)

countPositions :: Map -> Int
countPositions (Map m) = length . filter (== 'X') . unlines $ m

runGuard :: Guard -> Map -> Map
runGuard p m = case guardTurn p m of
    Just (newM, newP) -> runGuard newP newM
    Nothing -> m

-- >>> guardTurn (Position 1 1 T) (Map [".#.","...","..."])
-- Just (Map {getMap = [".#.",".X.","..."]},Position 1 2 T)
guardTurn :: Guard -> Map -> Maybe (Map, Guard)
guardTurn p m = do
    newPosition <- move p m
    newMap <- markPosition newPosition m
    return (newMap, newPosition)

-- >>> move (Position 2 1 T) (Map ["abc","abc","abc"])
-- >>> move (Position 0 1 L) (Map ["abc","abc","abc"])
-- Just (Position 2 0 T)
-- Nothing
move :: Guard -> Map -> Maybe Guard
move p m = do
    let newP = advance p
    s <- mapAt newP m
    return $ if s == '#' then turn p else newP

advance :: Guard -> Guard
advance (Guard (Position x y) d) = case d of
    L -> Guard (Position (x - 1) y) d
    T -> Guard (Position x (y + 1)) d
    R -> Guard (Position (x + 1) y) d
    D -> Guard (Position x (y - 1)) d

turn :: Guard -> Guard
turn (Guard (Position x y) d) = case d of
    L -> Guard (Position x y) T
    T -> Guard (Position x y) R
    R -> Guard (Position x y) D
    D -> Guard (Position x y) L

-- >>> markPosition (Position 1 2 T) (Map ["abc","abc","abc"])
-- Just (Map {getMap = ["abc","abc","aXc"]})
markPosition :: Guard -> Map -> Maybe Map
markPosition (Guard (Position x y) _) (Map m) = do
    row <- itemAt y m
    newRow <- setItemAt x 'X' row
    newMap <- setItemAt y newRow m
    return $ Map newMap

-- >>> mapAt (Position 1 2 T) (Map ["abc","abc","abc"])
-- Just 'b'
mapAt :: Guard -> Map -> Maybe Char
mapAt (Guard (Position x y) _) (Map m) = itemAt y m >>= itemAt x

-- >>> let example = "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."
-- >>> getInitPosition $ lines example
-- Just (Position 6 4 T)
getInitPosition :: [String] -> Maybe Guard
getInitPosition xs = do
    y <- findIndex (elem '^') xs
    row <- itemAt y xs
    x <- elemIndex '^' row
    return $ Guard (Position x y) T

{- | Part 2

While The Historians begin working around the guard's patrol route, you borrow
their fancy device and step outside the lab. From the safety of a supply closet,
you time travel through the last few months and record the nightly status of the
lab's guard post on the walls of the closet.

Returning after what seems like only a few seconds to The Historians, they
explain that the guard's patrol area is simply too large for them to safely
search the lab without getting caught.

Fortunately, they are pretty sure that adding a single new obstruction won't
cause a time paradox. They'd like to place the new obstruction in such a way
that the guard will get stuck in a loop, making the rest of the lab safe to
search.

To have the lowest chance of creating a time paradox, The Historians would like
to know all of the possible positions for such an obstruction. The new
obstruction can't be placed at the guard's starting position - the guard is
there right now and would notice.

In the above example, there are only 6 different positions where a new
obstruction would cause the guard to get stuck in a loop. The diagrams of these
six situations use O to mark the new obstruction, | to show a position where the
guard moves up/down, - to show a position where the guard moves left/right,
and + to show a position where the guard moves both up/down and left/right.

Option one, put a printing press next to the guard's starting position:

....#.....
....+---+#
....|...|.
..#.|...|.
....|..#|.
....|...|.
.#.O^---+.
........#.
#.........
......#...

Option two, put a stack of failed suit prototypes in the bottom right quadrant
of the mapped area:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
......O.#.
#.........
......#...

Option three, put a crate of chimney-squeeze prototype fabric next to the
standing desk in the bottom right quadrant:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
.+----+O#.
#+----+...
......#...

Option four, put an alchemical retroencabulator near the bottom left corner:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
..|...|.#.
#O+---+...
......#...

Option five, put the alchemical retroencabulator a bit to the right instead:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
....|.|.#.
#..O+-+...
......#...

Option six, put a tank of sovereign glue right next to the tank of universal
solvent:

....#.....
....+---+#
....|...|.
..#.|...|.
..+-+-+#|.
..|.|.|.|.
.#+-^-+-+.
.+----++#.
#+----++..
......#O..

It doesn't really matter what you choose to use as an obstacle so long as you
and The Historians can put it into position without the guard noticing. The
important thing is having enough options that you can find one that minimizes
time paradoxes, and in this example, there are 6 different positions you could
choose.

You need to get the guard stuck in a loop by adding a single new obstruction.
How many different positions could you choose for this obstruction?

Example should be 6:

>>> let example = "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."
>>> part2 example
6
-}
part2 :: String -> Int
part2 s = case getInitPosition i of
    Just g ->
        length
            . filter (\p -> isCycleMap g p m)
            . S.toList
            . traceGuard g S.empty
            $ m
    Nothing -> 0
  where
    i = reverse . lines $ s
    m = Map $ map (replace '^' 'X') i

traceGuard :: Guard -> S.Set Position -> Map -> S.Set Position
traceGuard g ps m = case guardTurn g m of
    Just (newM, newG@(Guard p _)) -> traceGuard newG (S.insert p ps) newM
    Nothing -> ps

isCycleMap :: Guard -> Position -> Map -> Bool
isCycleMap g p m = case putObstacle p m of
    Just om -> isCycle g S.empty om
    Nothing -> False

isCycle :: Guard -> S.Set Guard -> Map -> Bool
isCycle p ps m = case guardTurn p m of
    Just (newM, newP) ->
        S.member newP ps || isCycle newP (S.insert newP ps) newM
    Nothing -> False

putObstacle :: Position -> Map -> Maybe Map
putObstacle (Position x y) (Map m) = do
    row <- itemAt y m
    newRow <- setItemAt x '#' row
    newMap <- setItemAt y newRow m
    return $ Map newMap

solve :: IO ()
solve = do
    content <- readFile "./src/Year2024/data/day06.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
