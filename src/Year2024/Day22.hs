module Year2024.Day22 (solve) where

import Data.Bits (xor)

{- | Part 1

Example should be 37327623:

>>> let example = "1\n10\n100\n2024"
>>> part1 example
37327623
-}
part1 :: String -> Int
part1 = sum . map (nthSecret 2000 . read) . lines

-- >>> nthSecret 10 123
-- 5908254
nthSecret :: Int -> Int -> Int
nthSecret 0 seed = seed
nthSecret n seed = nthSecret (n - 1) (nextSecret seed)

-- >>> nextSecret 123
-- 15887950
nextSecret :: Int -> Int
nextSecret = step3 . step2 . step1

step1 :: Int -> Int
step1 s = prune . mix s $ s * 64

step2 :: Int -> Int
step2 s = prune . mix s $ s `div` 32

step3 :: Int -> Int
step3 s = prune . mix s $ s * 2048

-- >>> mix 42 15
-- 37
mix :: Int -> Int -> Int
mix s x = s `xor` x

-- >>> prune 100000000
-- 16113920
prune :: Int -> Int
prune s = s `mod` 16777216

{- | Part 2

Example:

>>> let example = ""
>>> part2 example
-}
part2 :: String -> Int
part2 = undefined

pInput :: String -> a
pInput = undefined

solve :: IO ()
solve = do
    -- example <- readFile "./src/Year2024/data/day22-example.txt"
    -- print $ "Part1 example: " ++ show (part1 example)
    -- print $ "Part2 example: " ++ show (part2 example)

    content <- readFile "./src/Year2024/data/day22.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
