module Year2024.Day23 (solve) where

{- | Part 1

Example:

>>> let example = ""
>>> part1 example
-}
part1 :: String -> Int
part1 = undefined

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
    -- example <- readFile "./src/Year2024/data/day23-example.txt"
    -- print $ "Part1 example: " ++ show (part1 example)
    -- print $ "Part2 example: " ++ show (part2 example)

    content <- readFile "./src/Year2024/data/day23.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
