module Year2024.Day11 (solve) where

import qualified Data.IntMap as IM
import qualified Data.IntMap as Im

{- | Part 1

Example should be 55312:

>>> let example = "125 17\n"
>>> part1 example
55312
-}
part1 :: String -> Int
part1 = length . advance 25 . map read . words . head . lines

advance :: Int -> [Int] -> [Int]
advance 0 xs = xs
advance r xs = advance (r - 1) (concatMap stoneChange xs)

stoneChange :: Int -> [Int]
stoneChange x
    | x == 0 = [1]
    | even . length . show $ x = [read . take l $ num, read . drop l $ num]
    | otherwise = [x * 2024]
  where
    num = show x
    l = length num `div` 2

{- | Part 2

Example should be 55312:

"Part2 solution: 225404711855335"

>>> let example = "125 17\n"
>>> part2 example
-}
part2 :: String -> Int
part2 =
    sum
        . map snd
        . IM.toList
        . advance' 75
        . Im.fromList
        . map (\d -> (read d, 1))
        . words
        . head
        . lines

advance' :: Int -> IM.IntMap Int -> IM.IntMap Int
advance' 0 im = im
advance' r im = advance' (r - 1) (blink' im)

blink' :: IM.IntMap Int -> IM.IntMap Int
blink' im = foldr (\(k, v) m -> IM.insertWith (+) k v m) IM.empty advancedList
  where
    advancedList = concatMap stoneChange' (IM.toList im)

stoneChange' :: (Int, Int) -> [(Int, Int)]
stoneChange' (key, val)
    | key == 0 = [(1, val)]
    | even . length . show $ key =
        [(read . take l $ num, val), (read . drop l $ num, val)]
    | otherwise = [(key * 2024, val)]
  where
    num = show key
    l = length num `div` 2

solve :: IO ()
solve = do
    content <- readFile "./src/Year2024/data/day11.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
