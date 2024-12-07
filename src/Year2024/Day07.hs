module Year2024.Day07 (solve) where

import Data.List.Split (splitOn)
import Utils (strip)

{- | Part 1

The Historians take you to a familiar rope bridge over a river in the middle of
a jungle. The Chief isn't on this side of the bridge, though; maybe he's on the
other side?

When you go to cross the bridge, you notice a group of engineers trying to
repair it. (Apparently, it breaks pretty frequently.) You won't be able to cross
until it's fixed.

You ask how long it'll take; the engineers tell you that it only needs final
calibrations, but some young elephants were playing nearby and stole all the
operators from their calibration equations! They could finish the calibrations
if only someone could determine which test values could possibly be produced by
placing any combination of operators into their calibration equations (your
puzzle input).

For example:

190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20

Each line represents a single equation. The test value appears before the colon
on each line; it is your job to determine whether the remaining numbers can be
combined with operators to produce the test value.

Operators are always evaluated left-to-right, not according to precedence rules.
Furthermore, numbers in the equations cannot be rearranged. Glancing into the
jungle, you can see elephants holding two different types of operators: add (+)
and multiply (*).

Only three of the above equations can be made true by inserting operators:

190: 10 19 has only one position that accepts an operator: between 10 and 19.
     Choosing + would give 29, but choosing * would give the test value
     (10 * 19 = 190).
3267: 81 40 27 has two positions for operators. Of the four possible
      configurations of the operators, two cause the right side to match the
      test value: 81 + 40 * 27 and 81 * 40 + 27 both equal 3267
      (when evaluated left-to-right)!
292: 11 6 16 20 can be solved in exactly one way: 11 + 6 * 16 + 20.

The engineers just need the total calibration result, which is the sum of the
test values from just the equations that could possibly be true. In the above
example, the sum of the test values for the three equations listed above is
3749.

Determine which equations could possibly be true. What is their total
calibration result?

Example should be 3749:

>>> let example = "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"
>>> part1 example
3749
-}
part1 :: String -> Int
part1 =
    sum
        . map getRes
        . filter (\eq -> isValid eq [(+), (*)])
        . pInput

data Equation = Equation Int [Int] deriving (Show)

getRes :: Equation -> Int
getRes (Equation res _) = res

-- >>> isValid (Equation 3267 [81,40,27]) [(+), (*)]
-- True
isValid :: Equation -> [Int -> Int -> Int] -> Bool
isValid (Equation 0 []) _ = True
isValid (Equation _ []) _ = False
isValid (Equation res (n : ns)) ops = elem res $ map (eval n ns) combOps
  where
    combOps = combinations (length ns) ops [[]]

-- >>> eval 81 [40,27] [(+), (*)]
-- 3267
eval :: Int -> [Int] -> [Int -> Int -> Int] -> Int
eval r [] _ = r
eval r _ [] = r
eval r (n : ns) (op : ops) = eval (op r n) ns ops

-- >>> combinations 3 [1,2] [[]]
-- >>> combinations 1 [1,2] [[]]
-- [[1,1,1],[2,1,1],[1,2,1],[2,2,1],[1,1,2],[2,1,2],[1,2,2],[2,2,2]]
-- [[1],[2]]
combinations :: Int -> [a] -> [[a]] -> [[a]]
combinations _ [] _ = []
combinations 0 _ cs = cs
combinations n xs cs = combinations (n - 1) xs newCs
  where
    newCs = concatMap (\l -> map (: l) xs) cs

{- | Part 2

The engineers seem concerned; the total calibration result you gave them is
nowhere close to being within safety tolerances. Just then, you spot your
mistake: some well-hidden elephants are holding a third type of operator.

The concatenation operator (||) combines the digits from its left and right
inputs into a single number. For example, 12 || 345 would become 12345. All
operators are still evaluated left-to-right.

Now, apart from the three equations that could be made true using only addition
and multiplication, the above example has three more equations that can be made
true by inserting operators:

156: 15 6 can be made true through a single concatenation: 15 || 6 = 156.
7290: 6 8 6 15 can be made true using 6 * 8 || 6 * 15.
192: 17 8 14 can be made true using 17 || 8 + 14.

Adding up all six test values (the three that could be made before using
only + and * plus the new three that can now be made by also using ||) produces
the new total calibration result of 11387.

Using your new knowledge of elephant hiding spots, determine which equations
could possibly be true. What is their total calibration result?

Example:

>>> let example = "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"
>>> part2 example
11387
-}
part2 :: String -> Int
part2 =
    sum
        . map getRes
        . filter (\eq -> isValid eq [(+), (*), concatOp])
        . pInput

-- >>> concatOp 12 345
-- 12345
concatOp :: Int -> Int -> Int
concatOp a b = read (show a ++ show b)

-- >>> pInput "190: 10 19\n3267: 81 40 27\n83: 17 5"
-- [Equation 190 [10,19],Equation 3267 [81,40,27],Equation 83 [17,5]]
pInput :: String -> [Equation]
pInput = map pEquation . lines

-- >>> pEquation "190: 10 19"
-- Equation 190 [10,19]
pEquation :: String -> Equation
pEquation s =
    Equation <$> read . head <*> map read . words . strip . last $ splitOn ":" s

solve :: IO ()
solve = do
    content <- readFile "./src/Year2024/data/day07.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
