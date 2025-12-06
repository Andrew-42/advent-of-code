module Year2025.Day06 (solve) where

import Data.List.Split (splitWhen)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Utils (strip, transpose)

{- | Part 1

--- Day 6: Trash Compactor ---

After helping the Elves in the kitchen, you were taking a break and helping them re-enact a movie
scene when you over-enthusiastically jumped into the garbage chute!

A brief fall later, you find yourself in a garbage smasher. Unfortunately, the door's been
magnetically sealed.

As you try to find a way out, you are approached by a family of cephalopods! They're pretty sure
they can get the door open, but it will take some time. While you wait, they're curious if you can
help the youngest cephalopod with her math homework.

Cephalopod math doesn't look that different from normal math. The math worksheet (your puzzle input)
consists of a list of problems; each problem has a group of numbers that need to be either added (+)
or multiplied (*) together.

However, the problems are arranged a little strangely; they seem to be presented next to each other
in a very long horizontal list. For example:

123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +

Each problem's numbers are arranged vertically; at the bottom of the problem is the symbol for the
operation that needs to be performed. Problems are separated by a full column of only spaces. The
left/right alignment of numbers within each problem can be ignored.

So, this worksheet contains four problems:

123 * 45 * 6 = 33210
328 + 64 + 98 = 490
51 * 387 * 215 = 4243455
64 + 23 + 314 = 401

To check their work, cephalopod students are given the grand total of adding together all of the
answers to the individual problems. In this worksheet, the grand total is
33210 + 490 + 4243455 + 401 = 4277556.

Of course, the actual worksheet is much wider. You'll need to make sure to unroll it completely so
that you can read the problems clearly.

Solve the problems on the math worksheet. What is the grand total found by adding together all of
the answers to the individual problems?

Your puzzle answer was 4951502530386.

Example:

>>> let example = "123 328  51 64\n 45 64  387 23\n  6 98  215 314\n*   +   *   +  "
>>> part1 example
4277556
-}
part1 :: T.Text -> Int
part1 = sum . map solve' . pInput

{- | Parser

Example:

>>> let example = "123 328  51 64\n 45 64  387 23\n  6 98  215 314\n*   +   *   +  "
>>> pInput example
[Problem {nums = [123,45,6], op = Mul},Problem {nums = [328,64,98], op = Add},Problem {nums = [51,387,215], op = Mul},Problem {nums = [64,23,314], op = Add}]
-}
pInput :: T.Text -> [Problem]
pInput = map parse . transpose . map T.words . T.lines
  where
    parse :: [T.Text] -> Problem
    parse xs = Problem (map readT $ init xs) op'
      where
        op' = case last xs of
            "+" -> Add
            "*" -> Mul
            _ -> error $ T.unpack ("Invalid operation: " <> last xs)

    readT :: (Read a) => T.Text -> a
    readT = read . T.unpack

{- | Part 2

The big cephalopods come back to check on how things are going. When they see that your grand total
doesn't match the one expected by the worksheet, they realize they forgot to explain how to read
cephalopod math.

Cephalopod math is written right-to-left in columns. Each number is given in its own column, with
the most significant digit at the top and the least significant digit at the bottom. (Problems are
still separated with a column consisting only of spaces, and the symbol at the bottom of the problem
is still the operator to use.)

Here's the example worksheet again:

123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +

Reading the problems right-to-left one column at a time, the problems are now quite different:

The rightmost problem is 4 + 431 + 623 = 1058
The second problem from the right is 175 * 581 * 32 = 3253600
The third problem from the right is 8 + 248 + 369 = 625
Finally, the leftmost problem is 356 * 24 * 1 = 8544
Now, the grand total is 1058 + 3253600 + 625 + 8544 = 3263827.

Solve the problems on the math worksheet again. What is the grand total found by adding together all
of the answers to the individual problems?

Your puzzle answer was 8486156119946.

Example:

>>> let example = "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  "
>>> part2 example
3263827
-}
part2 :: T.Text -> Int
part2 = sum . map solve' . pInput2

{- | Parser

Example:

>>> let example = "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  "
>>> pInput2 example
[Problem {nums = [1,24,356], op = Mul},Problem {nums = [369,248,8], op = Add},Problem {nums = [32,581,175], op = Mul},Problem {nums = [623,431,4], op = Add}]
-}
pInput2 :: T.Text -> [Problem]
pInput2 text = zipWith Problem nums' ops
  where
    input = lines . T.unpack $ text

    nums' :: [[Int]]
    nums' = map (map read) . splitWhen (== "") . map strip . transpose . init $ input
    ops = map parseOp . words . last $ input

    parseOp txt = case txt of
        "+" -> Add
        "*" -> Mul
        _ -> error $ "Invalid operation: " <> txt

data Op = Mul | Add deriving (Show)
data Problem = Problem {nums :: [Int], op :: Op} deriving (Show)

solve' :: Problem -> Int
solve' problem = case problem.op of
    Mul -> product problem.nums
    Add -> sum problem.nums

solve :: IO ()
solve = do
    content <- TIO.readFile "./src/Year2025/data/day06.txt"
    -- Part1 solution: 4951502530386
    print $ "Part1 solution: " ++ show (part1 content)
    -- Part2 solution: 8486156119946
    print $ "Part2 solution: " ++ show (part2 content)
