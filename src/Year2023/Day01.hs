module Year2023.Day01 where

import Data.Char (isDigit)
import Data.List (isPrefixOf, tails)

{- | Part 1

Something is wrong with global snow production, and you've been selected to take
a look. The Elves have even given you a map; on it, they've used stars to mark
the top fifty locations that are likely to be having problems.

You've been doing this long enough to know that to restore snow operations, you
need to check all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day
in the Advent calendar; the second puzzle is unlocked when you complete the
first. Each puzzle grants one star. Good luck!

You try to ask why they can't just use a weather machine ("not powerful enough")
and where they're even sending you ("the sky") and why your map looks mostly
blank ("you sure ask a lot of questions") and hang on did you just say the sky
("of course, where do you think snow comes from") when you realize that the
Elves are already loading you into a trebuchet ("please hold still, we need to
strap you in").

As they're making the final adjustments, they discover that their calibration
document (your puzzle input) has been amended by a very young Elf who was
apparently just excited to show off her art skills. Consequently, the Elves are
having trouble reading the values on the document.

The newly-improved calibration document consists of lines of text; each line
originally contained a specific calibration value that the Elves now need to
recover. On each line, the calibration value can be found by combining the first
digit and the last digit (in that order) to form a single two-digit number.

Example should evaluate to 142:

>>> let example = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
>>> part1 example
142
-}
part1 :: String -> Int
part1 =
    sum
        . map (read . (\xs -> [head xs, last xs]) . filter isDigit)
        . lines

{- | Part 2

Your calculation isn't quite right. It looks like some of the digits are
actually spelled out with letters: one, two, three, four, five, six, seven,
eight, and nine also count as valid "digits".

Example:

>>> let example = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
>>> part2 example
281
-}
part2 :: String -> Int
part2 =
    sum
        . map (toInt . filter (/= "") . map stringToDigit . tails)
        . lines
  where
    toInt :: [String] -> Int
    toInt xs = read (head xs) * 10 + read (last xs)

stringToDigit :: String -> String
stringToDigit s
    | "one" `isPrefixOf` s = "1"
    | "two" `isPrefixOf` s = "2"
    | "three" `isPrefixOf` s = "3"
    | "four" `isPrefixOf` s = "4"
    | "five" `isPrefixOf` s = "5"
    | "six" `isPrefixOf` s = "6"
    | "seven" `isPrefixOf` s = "7"
    | "eight" `isPrefixOf` s = "8"
    | "nine" `isPrefixOf` s = "9"
    | "1" `isPrefixOf` s = "1"
    | "2" `isPrefixOf` s = "2"
    | "3" `isPrefixOf` s = "3"
    | "4" `isPrefixOf` s = "4"
    | "5" `isPrefixOf` s = "5"
    | "6" `isPrefixOf` s = "6"
    | "7" `isPrefixOf` s = "7"
    | "8" `isPrefixOf` s = "8"
    | "9" `isPrefixOf` s = "9"
    | otherwise = ""

solve :: IO ()
solve = do
    content <- readFile "./src/Year2023/data/day01.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
