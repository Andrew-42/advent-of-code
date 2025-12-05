module Year2025.Day07 (solve) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

{- | Part 1

Example:

>>> let example = ""
>>> part1 example
-}
part1 :: T.Text -> Int
part1 = undefined . pInput

{- | Part 2

Example:

>>> let example = ""
>>> part2 example
-}
part2 :: T.Text -> Int
part2 = undefined . pInput

pInput :: T.Text -> a
pInput = undefined

solve :: IO ()
solve = do
    content <- TIO.readFile "./src/Year2025/data/day07.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
