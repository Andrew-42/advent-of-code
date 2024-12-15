module Year2024.Day15 (solve) where

import Data.Foldable (foldl', foldlM)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Grid (
    Grid2D (Grid2D),
    Position (Position),
    gridAt,
    positionOf,
    positionsOf,
    setGridAt,
 )

{- | Part 1

You appear back inside your own mini submarine! Each Historian drives their mini
submarine in a different direction; maybe the Chief has his own submarine down
here somewhere as well?

You look up to see a vast school of lanternfish swimming past you. On closer
inspection, they seem quite anxious, so you drive your mini submarine over to
see if you can help.

Because lanternfish populations grow rapidly, they need a lot of food, and that
food needs to be stored somewhere. That's why these lanternfish have built
elaborate warehouse complexes operated by robots!

These lanternfish seem so anxious because they have lost control of the robot
that operates one of their most important warehouses! It is currently running
amok, pushing around boxes in the warehouse with no regard for lanternfish
logistics or lanternfish inventory management strategies.

Right now, none of the lanternfish are brave enough to swim up to an
unpredictable robot so they could shut it off. However, if you could anticipate
the robot's movements, maybe they could find a safe option.

The lanternfish already have a map of the warehouse and a list of movements the
robot will attempt to make (your puzzle input). The problem is that the
movements will sometimes fail as boxes are shifted around, making the actual
movements of the robot difficult to predict.

For example:

##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^

As the robot (@) attempts to move, if there are any boxes (O) in the way, the
robot will also attempt to push those boxes. However, if this action would cause
the robot or a box to move into a wall (#), nothing moves instead, including the
robot. The initial positions of these are shown on the map at the top of the
document the lanternfish gave you.

The rest of the document describes the moves (^ for up, v for down, < for left,
> for right) that the robot will attempt to make, in order. (The moves form a
single giant sequence; they are broken into multiple lines just to make
copy-pasting easier. Newlines within the move sequence should be ignored.)

Here is a smaller example to get started:

########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<

Were the robot to attempt the given sequence of moves, it would push around the
boxes as follows:

Initial state:
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Move <:
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Move ^:
########
#.@O.O.#
##..O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Move ^:
########
#.@O.O.#
##..O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Move >:
########
#..@OO.#
##..O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Move >:
########
#...@OO#
##..O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Move >:
########
#...@OO#
##..O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Move v:
########
#....OO#
##..@..#
#...O..#
#.#.O..#
#...O..#
#...O..#
########

Move v:
########
#....OO#
##..@..#
#...O..#
#.#.O..#
#...O..#
#...O..#
########

Move <:
########
#....OO#
##.@...#
#...O..#
#.#.O..#
#...O..#
#...O..#
########

Move v:
########
#....OO#
##.....#
#..@O..#
#.#.O..#
#...O..#
#...O..#
########

Move >:
########
#....OO#
##.....#
#...@O.#
#.#.O..#
#...O..#
#...O..#
########

Move >:
########
#....OO#
##.....#
#....@O#
#.#.O..#
#...O..#
#...O..#
########

Move v:
########
#....OO#
##.....#
#.....O#
#.#.O@.#
#...O..#
#...O..#
########

Move <:
########
#....OO#
##.....#
#.....O#
#.#O@..#
#...O..#
#...O..#
########

Move <:
########
#....OO#
##.....#
#.....O#
#.#O@..#
#...O..#
#...O..#
########

The larger example has many more moves; after the robot has finished those
moves, the warehouse would look like this:

##########
#.O.O.OOO#
#........#
#OO......#
#OO@.....#
#O#.....O#
#O.....OO#
#O.....OO#
#OO....OO#
##########

The lanternfish use their own custom Goods Positioning System (GPS for short) to
track the locations of the boxes. The GPS coordinate of a box is equal to 100
times its distance from the top edge of the map plus its distance from the left
edge of the map. (This process does not stop at wall tiles; measure all the way
to the edges of the map.)

So, the box shown below has a distance of 1 from the top edge of the map and 4
from the left edge of the map, resulting in a GPS coordinate of
100 * 1 + 4 = 104.

#######
#...O..
#......

The lanternfish would like to know the sum of all boxes' GPS coordinates after
the robot finishes moving. In the larger example, the sum of all boxes' GPS
coordinates is 10092. In the smaller example, the sum is 2028.

Predict the motion of the robot and boxes in the warehouse. After the robot is
finished moving, what is the sum of all boxes' GPS coordinates?

Example1 should be 2028:

>>> let example = "########\n#..O.O.#\n##@.O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########\n\n<^^>>>vv<v>>v<<"
>>> part1 example
2028
-}
part1 :: String -> Int
part1 s = case advanceRobot ds g of
    Just g' -> sum . map coordinates . positionsOf 'O' $ g'
    Nothing -> 0
  where
    (g, ds) = pInput s

data Direction = T | R | D | L deriving (Show, Eq)

coordinates :: Position -> Int
coordinates (Position x y) = x + 100 * y

advanceRobot :: [Direction] -> Grid2D Char -> Maybe (Grid2D Char)
advanceRobot ds g = do
    r <- positionOf '@' g
    ng <- setGridAt '.' r g
    let (_, ng') = foldl' (\(r', g') d -> step r' d g') (r, ng) ds
    return ng'

-- >>> step (Position 0 0) R (Grid2D [".OOO."])
-- >>> step (Position 0 0) R (Grid2D [".O.O."])
-- >>> step (Position 1 0) R (Grid2D [".OOO#"])
-- (Position 1 0,Grid2D ["..OOO"])
-- (Position 1 0,Grid2D ["..OO."])
-- (Position 1 0,Grid2D [".OOO#"])
step :: Position -> Direction -> Grid2D Char -> (Position, Grid2D Char)
step p d g = fromMaybe (p, g) (step' p d g)

step' :: Position -> Direction -> Grid2D Char -> Maybe (Position, Grid2D Char)
step' p d g = case gridAt np g of
    Just 'O' -> do
        mg <- moveBox np d g
        ng <- setGridAt '.' np mg
        pure (np, ng)
    Just '#' -> Nothing
    Just '.' -> Just (np, g)
    Just _ -> Nothing
    Nothing -> Nothing
  where
    np = next p d

-- >>> moveBox (Position 1 0) R (Grid2D [".OOO."])
-- >>> moveBox (Position 1 0) R (Grid2D [".OOO#"])
-- Just (Grid2D [".OOOO"])
-- Nothing
moveBox :: Position -> Direction -> Grid2D Char -> Maybe (Grid2D Char)
moveBox p d g
    | isBox np g = moveBox np d g
    | isWall np g = Nothing
    | otherwise = setGridAt 'O' np g
  where
    np = next p d

isBox :: Position -> Grid2D Char -> Bool
isBox p g = case gridAt p g of
    Just 'O' -> True
    _ -> False

isWall :: Position -> Grid2D Char -> Bool
isWall p g = case gridAt p g of
    Just '#' -> True
    _ -> False

next :: Position -> Direction -> Position
next (Position x y) T = Position x (y + 1)
next (Position x y) R = Position (x + 1) y
next (Position x y) D = Position x (y - 1)
next (Position x y) L = Position (x - 1) y

{- | Part 2

The lanternfish use your information to find a safe moment to swim in and turn
off the malfunctioning robot! Just as they start preparing a festival in your
honor, reports start coming in that a second warehouse's robot is also
malfunctioning.

This warehouse's layout is surprisingly similar to the one you just helped.
There is one key difference: everything except the robot is twice as wide! The
robot's list of movements doesn't change.

To get the wider warehouse's map, start with your original map and, for each
tile, make the following changes:

If the tile is #, the new map contains ## instead.
If the tile is O, the new map contains [] instead.
If the tile is ., the new map contains .. instead.
If the tile is @, the new map contains @. instead.

This will produce a new warehouse map which is twice as wide and with wide boxes
that are represented by []. (The robot does not change size.)

The larger example from before would now look like this:

####################
##....[]....[]..[]##
##............[]..##
##..[][]....[]..[]##
##....[]@.....[]..##
##[]##....[]......##
##[]....[]....[]..##
##..[][]..[]..[][]##
##........[]......##
####################

Because boxes are now twice as wide but the robot is still the same size and
speed, boxes can be aligned such that they directly push two other boxes at
once. For example, consider this situation:

#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^

After appropriately resizing this map, the robot would push around these boxes
as follows:

Initial state:
##############
##......##..##
##..........##
##....[][]@.##
##....[]....##
##..........##
##############

Move <:
##############
##......##..##
##..........##
##...[][]@..##
##....[]....##
##..........##
##############

Move v:
##############
##......##..##
##..........##
##...[][]...##
##....[].@..##
##..........##
##############

Move v:
##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##.......@..##
##############

Move <:
##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##......@...##
##############

Move <:
##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##.....@....##
##############

Move ^:
##############
##......##..##
##...[][]...##
##....[]....##
##.....@....##
##..........##
##############

Move ^:
##############
##......##..##
##...[][]...##
##....[]....##
##.....@....##
##..........##
##############

Move <:
##############
##......##..##
##...[][]...##
##....[]....##
##....@.....##
##..........##
##############

Move <:
##############
##......##..##
##...[][]...##
##....[]....##
##...@......##
##..........##
##############

Move ^:
##############
##......##..##
##...[][]...##
##...@[]....##
##..........##
##..........##
##############

Move ^:
##############
##...[].##..##
##...@.[]...##
##....[]....##
##..........##
##..........##
##############

This warehouse also uses GPS to locate the boxes. For these larger boxes,
distances are measured from the edge of the map to the closest edge of the box
in question. So, the box shown below has a distance of 1 from the top edge of
the map and 5 from the left edge of the map, resulting in a GPS coordinate of
100 * 1 + 5 = 105.

##########
##...[]...
##........

In the scaled-up version of the larger example from above, after the robot has
finished all of its moves, the warehouse would look like this:

####################
##[].......[].[][]##
##[]...........[].##
##[]........[][][]##
##[]......[]....[]##
##..##......[]....##
##..[]............##
##..@......[].[][]##
##......[][]..[]..##
####################

The sum of these boxes' GPS coordinates is 9021.

Predict the motion of the robot and boxes in this new, scaled-up warehouse. What
is the sum of all boxes' final GPS coordinates?

Example:

>>> let example = "#######\n#...#.#\n#.....#\n#..OO@#\n#..O..#\n#.....#\n#######\n\n <vv<<^^<<^^"
>>> part2 example
618
-}
part2 :: String -> Int
part2 s = case advanceRobot' ds bg of
    Just g' -> sum . map coordinates . positionsOf '[' $ g'
    Nothing -> 0
  where
    (g, ds) = pInput s
    bg = growGrid g

advanceRobot' :: [Direction] -> Grid2D Char -> Maybe (Grid2D Char)
advanceRobot' ds g = do
    r <- positionOf '@' g
    ng <- setGridAt '.' r g
    let (_, ng') = foldl' (\(r', g') d -> step2 r' d g') (r, ng) ds
    return ng'

step2 :: Position -> Direction -> Grid2D Char -> (Position, Grid2D Char)
step2 p d g = fromMaybe (p, g) (step2' p d g)

-- >>> step2' (Position 5 0) L (Grid2D [".[][]."])
-- >>> step2' (Position 2 3) D (Grid2D [".....",".[][].", "..[]..","....."])
-- Just (Position 4 0,Grid2D ["[][].."])
-- Just (Position 2 2,Grid2D [".[][]","..[]..","......","....."])
step2' :: Position -> Direction -> Grid2D Char -> Maybe (Position, Grid2D Char)
step2' p d g = case gridAt np g of
    Just '#' -> Nothing
    Just '.' -> Just (np, g)
    Just _ -> case d of
        T -> moveBoxV np T g >>= (\ng -> Just (np, ng))
        D -> moveBoxV np D g >>= (\ng -> Just (np, ng))
        R -> moveBoxR np g >>= setGridAt '.' np >>= (\ng -> Just (np, ng))
        L -> moveBoxL np g >>= setGridAt '.' np >>= (\ng -> Just (np, ng))
    Nothing -> Nothing
  where
    np = next p d

-- >>> moveBoxL (Position 4 0) (Grid2D ["...[]."])
-- >>> moveBoxL (Position 4 0) (Grid2D [".[][]."])
-- >>> moveBoxL (Position 4 0) (Grid2D ["#[][]."])
-- Just (Grid2D ["..[]]."])
-- Just (Grid2D ["[][]]."])
-- Nothing
moveBoxL :: Position -> Grid2D Char -> Maybe (Grid2D Char)
moveBoxL p g = case gridAt np g of
    Just '.' -> ng
    Just ']' -> ng >>= moveBoxL np
    Just '#' -> Nothing
    Just _ -> Nothing
    Nothing -> Nothing
  where
    e = next p L
    np = next e L
    ng = setGridAt '[' np =<< setGridAt ']' e g

-- >>> moveBoxR (Position 1 0) (Grid2D [".[]..."])
-- Just (Grid2D [".[[].."])
moveBoxR :: Position -> Grid2D Char -> Maybe (Grid2D Char)
moveBoxR p g = case gridAt np g of
    Just '.' -> ng
    Just '[' -> ng >>= moveBoxR np
    Just '#' -> Nothing
    Just _ -> Nothing
    Nothing -> Nothing
  where
    e = next p R
    np = next e R
    ng = setGridAt '[' e g >>= setGridAt ']' np

-- >>> moveBoxV (Position 2 0) T (Grid2D ["..[]..","..[]..", "......"])
-- >>> moveBoxV (Position 2 0) T (Grid2D ["..[]..",".[][].", "......"])
-- >>> moveBoxV (Position 2 0) T (Grid2D ["..[]..",".[][].", "....#."])
-- >>> moveBoxV (Position 2 2) D (Grid2D ["......","..[]..", "..[].."])
-- >>> moveBoxV (Position 2 2) D (Grid2D ["......",".[][].", "..[].."])
-- >>> moveBoxV (Position 2 2) D (Grid2D [".#....",".[][].", "..[].."])
-- Just (Grid2D ["......","..[]..","..[].."])
-- Just (Grid2D ["......","..[]..",".[][]."])
-- Nothing
-- Just (Grid2D ["..[]..","..[]..","......"])
-- Just (Grid2D [".[][].","..[]..","......"])
-- Nothing
moveBoxV :: Position -> Direction -> Grid2D Char -> Maybe (Grid2D Char)
moveBoxV p d g = do
    eraseD <- case d of
        D -> Just T
        T -> Just D
        _ -> Nothing
    ps <- boxPositions p d g
    let ups = S.toList $ S.fromList ps
    eraseGrid <- foldlM (flip eraseBox) g (map (`next` eraseD) ups)
    foldlM (flip writeBox) eraseGrid ups

writeBox :: Position -> Grid2D Char -> Maybe (Grid2D Char)
writeBox pr g = setGridAt ']' pr g >>= setGridAt '[' (next pr L)

eraseBox :: Position -> Grid2D Char -> Maybe (Grid2D Char)
eraseBox pr g = case gridAt pr g of
    Just ']' -> setGridAt '.' pr g >>= setGridAt '.' (next pr L)
    Just '[' -> setGridAt '.' pr g >>= setGridAt '.' (next pr R)
    _ -> Nothing

-- >>> boxPositions (Position 2 2) D (Grid2D ["......",".[][].", "..[].."])
-- Just [Position 3 1,Position 2 0,Position 4 0]
boxPositions :: Position -> Direction -> Grid2D Char -> Maybe [Position]
boxPositions p d g = case gridAt p g of
    Just '[' -> case sequenceA [gridAt dp g, gridAt dpR g] of
        Just ['[', ']'] -> case boxPositions dpR d g of
            Just ps -> Just $ dpR : ps
            Nothing -> Nothing
        Just [']', '['] -> case sequenceA [boxPositions dp d g, boxPositions dpR d g] of
            Just [ps1, ps2] -> Just ([dpR] ++ ps1 ++ ps2)
            Just _ -> Nothing
            Nothing -> Nothing
        Just ['.', '['] -> case boxPositions dpR d g of
            Just ps -> Just $ dpR : ps
            Nothing -> Nothing
        Just [']', '.'] -> case boxPositions dp d g of
            Just ps -> Just $ dpR : ps
            Nothing -> Nothing
        Just ['.', '.'] -> Just [dpR]
        Just _ -> Nothing
        Nothing -> Nothing
    Just ']' -> case sequenceA [gridAt dpL g, gridAt dp g] of
        Just ['[', ']'] -> case boxPositions dp d g of
            Just ps -> Just $ dp : ps
            Nothing -> Nothing
        Just [']', '['] -> case sequenceA [boxPositions dpL d g, boxPositions dp d g] of
            Just [ps1, ps2] -> Just ([dp] ++ ps1 ++ ps2)
            Just _ -> Nothing
            Nothing -> Nothing
        Just ['.', '['] -> case boxPositions dp d g of
            Just ps -> Just $ dp : ps
            Nothing -> Nothing
        Just [']', '.'] -> case boxPositions dpL d g of
            Just ps -> Just $ dp : ps
            Nothing -> Nothing
        Just ['.', '.'] -> Just [dp]
        Just _ -> Nothing
        Nothing -> Nothing
    Just _ -> Nothing
    Nothing -> Nothing
  where
    dp = next p d
    dpL = next dp L
    dpR = next dp R

growGrid :: Grid2D Char -> Grid2D Char
growGrid (Grid2D ls) = Grid2D $ map grow ls
  where
    grow :: String -> String
    grow "" = ""
    grow (c : rest)
        | c == '#' = "##" ++ grow rest
        | c == 'O' = "[]" ++ grow rest
        | c == '.' = ".." ++ grow rest
        | c == '@' = "@." ++ grow rest
        | otherwise = error "Invalid map char: " ++ [c]

pInput :: String -> (Grid2D Char, [Direction])
pInput s = (Grid2D (head sps), ds)
  where
    sps = splitOn [""] . lines $ s
    ds = mapMaybe parseDirection . unlines . last $ sps

parseDirection :: Char -> Maybe Direction
parseDirection '^' = Just D
parseDirection '>' = Just R
parseDirection 'v' = Just T
parseDirection '<' = Just L
parseDirection _ = Nothing

solve :: IO ()
solve = do
    example <- readFile "./src/Year2024/data/day15-test.txt"
    print $ "Part1 example: " ++ show (part1 example)
    print $ "Part2 example: " ++ show (part2 example)

    content <- readFile "./src/Year2024/data/day15.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
