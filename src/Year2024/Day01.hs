module Year2024.Day01 (solve) where

import Data.List (sort)

{- | Part 1

The Chief Historian is always present for the big Christmas sleigh launch, but
nobody has seen him in months! Last anyone heard, he was visiting locations that
are historically significant to the North Pole; a group of Senior Historians has
asked you to accompany them as they check the places they think he was most
likely to visit.

As each location is checked, they will mark it on their list with a star. They
figure the Chief Historian must be in one of the first fifty places they'll
look, so in order to save Christmas, you need to help them get fifty stars on
their list before Santa takes off on December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day
in the Advent calendar; the second puzzle is unlocked when you complete the
first. Each puzzle grants one star. Good luck!

You haven't even left yet and the group of Elvish Senior Historians has already
hit a problem: their list of locations to check is currently empty. Eventually,
someone decides that the best place to check first would be the Chief
Historian's office.

Upon pouring into the office, everyone confirms that the Chief Historian is
indeed nowhere to be found. Instead, the Elves discover an assortment of notes
and lists of historically significant locations! This seems to be the planning
the Chief Historian was doing before he left. Perhaps these notes can be used to
determine which locations to search?

Throughout the Chief's office, the historically significant locations are listed
not by name but by a unique number called the location ID. To make sure they
don't miss anything, The Historians split into two groups, each searching the
office and trying to create their own complete list of location IDs.

There's just one problem: by holding the two lists up side by side (your puzzle
input), it quickly becomes clear that the lists aren't very similar. Maybe you
can help The Historians reconcile their lists?

Example should be 11:

>>> let example = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
>>> part1 example
11
-}
part1 :: String -> Int
part1 s = sum $ zipWith (\a b -> abs (a - b)) ls rs
  where
    (ls, rs) = pInput s

{- | Part 2

Your analysis only confirmed what everyone feared: the two lists of location IDs
are indeed very different.

Or are they?

The Historians can't agree on which group made the mistakes or how to read most
of the Chief's handwriting, but in the commotion you notice an interesting
detail: a lot of location IDs appear in both lists! Maybe the other numbers
aren't location IDs at all but rather misinterpreted handwriting.

This time, you'll need to figure out exactly how often each number from the left
list appears in the right list. Calculate a total similarity score by adding up
each number in the left list after multiplying it by the number of times that
number appears in the right list.

Example should be 31:

>>> let example = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
>>> part2 example
31
-}
part2 :: String -> Int
part2 c = sum . map (\num -> num * numCount num) $ ls
  where
    (ls, rs) = pInput c

    numCount :: Int -> Int
    numCount num = length $ filter (== num) rs

pInput :: String -> ([Int], [Int])
pInput i = (sort ls, sort rs)
  where
    (ls, rs) = unzip . map intPair . lines $ i

    intPair :: String -> (Int, Int)
    intPair s = (\ws -> (read $ head ws, read $ last ws)) $ words s

solve :: IO ()
solve = do
    content <- readFile "./src/Year2024/data/day01.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
