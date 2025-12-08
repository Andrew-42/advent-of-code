module Year2025.Day08 (solve) where

import Data.List (find, sortBy, sortOn)
import Data.Ord (Down (Down), comparing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Utils (pairs, removeFirst)

{- | Part 1

--- Day 8: Playground ---

Equipped with a new understanding of teleporter maintenance, you confidently step onto the repaired
teleporter pad.

You rematerialize on an unfamiliar teleporter pad and find yourself in a vast underground space
which contains a giant playground!

Across the playground, a group of Elves are working on setting up an ambitious Christmas decoration
project. Through careful rigging, they have suspended a large number of small electrical junction
boxes.

Their plan is to connect the junction boxes with long strings of lights. Most of the junction boxes
don't provide electricity; however, when two junction boxes are connected by a string of lights,
electricity can pass between those two junction boxes.

The Elves are trying to figure out which junction boxes to connect so that electricity can reach
every junction box. They even have a list of all of the junction boxes' positions in 3D space (your
puzzle input).

For example:

162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689

This list describes the position of 20 junction boxes, one per line. Each position is given as X,Y,Z
coordinates. So, the first junction box in the list is at X=162, Y=817, Z=812.

To save on string lights, the Elves would like to focus on connecting pairs of junction boxes that
are as close together as possible according to straight-line distance. In this example, the two
junction boxes which are closest together are 162,817,812 and 425,690,689.

By connecting these two junction boxes together, because electricity can flow between them, they
become part of the same circuit. After connecting them, there is a single circuit which contains two
junction boxes, and the remaining 18 junction boxes remain in their own individual circuits.

Now, the two junction boxes which are closest together but aren't already directly connected are
162,817,812 and 431,825,988. After connecting them, since 162,817,812 is already connected to
another junction box, there is now a single circuit which contains three junction boxes and an
additional 17 circuits which contain one junction box each.

The next two junction boxes to connect are 906,360,560 and 805,96,715. After connecting them, there
is a circuit containing 3 junction boxes, a circuit containing 2 junction boxes, and 15 circuits
which contain one junction box each.

The next two junction boxes are 431,825,988 and 425,690,689. Because these two junction boxes were
already in the same circuit, nothing happens!

This process continues for a while, and the Elves are concerned that they don't have enough
extension cables for all these circuits. They would like to know how big the circuits will be.

After making the ten shortest connections, there are 11 circuits: one circuit which contains 5
junction boxes, one circuit which contains 4 junction boxes, two circuits which contain 2 junction
boxes each, and seven circuits which each contain a single junction box. Multiplying together the
sizes of the three largest circuits (5, 4, and one of the circuits of size 2) produces 40.

Your list contains many junction boxes; connect together the 1000 pairs of junction boxes which are
closest together. Afterward, what do you get if you multiply together the sizes of the three largest
circuits?

Example:

>>> let example = "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689"
>>> part1 3 10 example
40
-}
part1 :: Int -> Int -> T.Text -> Int
part1 nC nD =
    product
        . take nC
        . sortBy (comparing Data.Ord.Down)
        . map S.size
        . connect [] nD
        . sortOn (uncurry distance)
        . pairs
        . pInput
  where
    connect :: [S.Set JunctionBox] -> Int -> [(JunctionBox, JunctionBox)] -> [S.Set JunctionBox]
    connect cs 0 _ = cs
    connect cs _ [] = cs
    connect cs n (b : bs) = connect (addBoxes cs b) (n - 1) bs

addBoxes :: [S.Set JunctionBox] -> (JunctionBox, JunctionBox) -> [S.Set JunctionBox]
addBoxes [] (jb1, jb2) = [S.fromList [jb1, jb2]]
addBoxes cs (jb1, jb2) = case (cJb1, cJb2) of
    (Nothing, Nothing) -> S.fromList [jb1, jb2] : cs
    (Nothing, Just c) -> S.insert jb1 c : removeFirst c cs
    (Just c, Nothing) -> S.insert jb2 c : removeFirst c cs
    (Just c1, Just c2) -> S.union c1 c2 : filter (\c -> c /= c1 && c /= c2) cs
  where
    cJb1 = find (S.member jb1) cs
    cJb2 = find (S.member jb2) cs

{- | Part 2

The Elves were right; they definitely don't have enough extension cables. You'll need to keep
connecting junction boxes together until they're all in one large circuit.

Continuing the above example, the first connection which causes all of the junction boxes to form a
single circuit is between the junction boxes at 216,146,977 and 117,168,530. The Elves need to know
how far those junction boxes are from the wall so they can pick the right extension cable;
multiplying the X coordinates of those two junction boxes (216 and 117) produces 25272.

Continue connecting the closest unconnected pairs of junction boxes together until they're all in
the same circuit. What do you get if you multiply together the X coordinates of the last two
junction boxes you need to connect?

Example:

>>> let example = "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689"
>>> part2 example
25272
-}
part2 :: T.Text -> Int
part2 text = wallDist $ connect (length jbs) [] (head pairJB) pairJB
  where
    wallDist :: (JunctionBox, JunctionBox) -> Int
    wallDist (jb1, jb2) = jb1.getX * jb2.getX

    jbs = pInput text
    pairJB = sortOn (uncurry distance) . pairs $ jbs

    connect ::
        Int ->
        [S.Set JunctionBox] ->
        (JunctionBox, JunctionBox) ->
        [(JunctionBox, JunctionBox)] ->
        (JunctionBox, JunctionBox)
    connect _ _ jbs' [] = jbs'
    connect total cs jbs' (b : bs)
        | length cs == 1 && (S.size (head cs) == total) = jbs'
        | otherwise = connect total (addBoxes cs b) b bs

{- | Parser

Example:

>>> let example = "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689"
>>> pInput example
[JunctionBox {getX = 162, getY = 817, getZ = 812},JunctionBox {getX = 57, getY = 618, getZ = 57},JunctionBox {getX = 906, getY = 360, getZ = 560},JunctionBox {getX = 592, getY = 479, getZ = 940},JunctionBox {getX = 352, getY = 342, getZ = 300},JunctionBox {getX = 466, getY = 668, getZ = 158},JunctionBox {getX = 542, getY = 29, getZ = 236},JunctionBox {getX = 431, getY = 825, getZ = 988},JunctionBox {getX = 739, getY = 650, getZ = 466},JunctionBox {getX = 52, getY = 470, getZ = 668},JunctionBox {getX = 216, getY = 146, getZ = 977},JunctionBox {getX = 819, getY = 987, getZ = 18},JunctionBox {getX = 117, getY = 168, getZ = 530},JunctionBox {getX = 805, getY = 96, getZ = 715},JunctionBox {getX = 346, getY = 949, getZ = 466},JunctionBox {getX = 970, getY = 615, getZ = 88},JunctionBox {getX = 941, getY = 993, getZ = 340},JunctionBox {getX = 862, getY = 61, getZ = 35},JunctionBox {getX = 984, getY = 92, getZ = 344},JunctionBox {getX = 425, getY = 690, getZ = 689}]
-}
pInput :: T.Text -> [JunctionBox]
pInput = map parseJB . T.lines
  where
    parseJB line = case T.splitOn "," line of
        [x, y, z] -> JunctionBox (readT x) (readT y) (readT z)
        _ -> error "Invalid junction box position input."
    readT = read . T.unpack

data JunctionBox = JunctionBox {getX :: Int, getY :: Int, getZ :: Int} deriving (Show, Eq, Ord)

distance :: JunctionBox -> JunctionBox -> Double
distance jb1 jb2 =
    sqrt . fromIntegral . sum . map pow2 $
        [jb2.getX - jb1.getX, jb2.getY - jb1.getY, jb2.getZ - jb1.getZ]
  where
    pow2 :: Int -> Int
    pow2 x = x * x

solve :: IO ()
solve = do
    content <- TIO.readFile "./src/Year2025/data/day08.txt"
    -- Part1 solution: 75582
    print $ "Part1 solution: " ++ show (part1 3 1000 content)
    -- Part2 solution: 59039696
    print $ "Part2 solution: " ++ show (part2 content)
