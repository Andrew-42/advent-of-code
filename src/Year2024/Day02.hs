module Year2024.Day02 (solve) where

import Data.List (inits, sort, tails)

{- | Part 1

Example should be 2:

>>> let example = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"
>>> part1 example
2
-}
part1 :: String -> Int
part1 = length . filter isSafeLevel . map (map read . words) . lines

isSafeLevel :: [Int] -> Bool
isSafeLevel xs = isSorted xs && isSafeLeap xs

isSorted :: [Int] -> Bool
isSorted xs = xs == sort xs || reverse xs == sort xs

isSafeLeap :: [Int] -> Bool
isSafeLeap xs = all inRange $ zipWith (\a b -> abs (a - b)) xs $ tail xs
  where
    inRange x = 1 <= x && x <= 3

{- | Part 2

Example should be 4:

>>> let example = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"
>>> part2 example
4
-}
part2 :: String -> Int
part2 s = part1 s + (length . filter isSafeWithouOne . unsafe $ s)
  where
    unsafe :: String -> [[Int]]
    unsafe = filter (not . isSafeLevel) . map (map read . words) . lines

isSafeWithouOne :: [Int] -> Bool
isSafeWithouOne = any isSafeLevel . genListsWithouOne

-- >>> genListsWithouOne [1, 2, 3, 4, 5]
genListsWithouOne :: [a] -> [[a]]
genListsWithouOne xs =
    zipWith (++) (map init . tail . inits $ xs) (map tail . init . tails $ xs)

solve :: IO ()
solve = do
    content <- readFile "./src/Year2024/data/day02.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
