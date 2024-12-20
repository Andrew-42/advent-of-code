module Year2024.Day19 (solve) where

import Data.List (isPrefixOf, sortBy)
import Data.List.Split (splitOn)
import Utils (strip)

{- | Part 1

Example:

>>> let example = "r, wr, b, g, bwu, rb, gb, br\n\nbrwrr\nbggr\ngbbr\nrrbgbr\nubwu\nbwurrg\nbrgr\nbbrgwb"
>>> part1 example
6
-}
part1 :: String -> Int
part1 s = length . filter (`isPossible` fTowels) $ patterns
  where
    (towels, patterns) = pInput s
    fTowels =
        filterTowels []
            . sortBy (\a b -> compare (length a) (length b))
            $ towels

-- >>> filterTowels [] ["r","wr","b","g","bwu","rb","gb","br"]
-- ["bwu","g","b","wr","r"]
filterTowels :: [String] -> [String] -> [String]
filterTowels rts [] = rts
filterTowels rts (t : ts) =
    if isPossible (Pattern t) rts
        then filterTowels rts ts
        else filterTowels (t : rts) ts

newtype Pattern = Pattern String deriving (Show, Eq)

-- data SearchP a = SearchP a (a -> S.Set a) (a -> Bool)

-- >>> isPossible (Pattern "bbrgwb") ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"]
-- >>> isPossible (Pattern "bggr") ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"]
-- False
-- True
isPossible :: Pattern -> [String] -> Bool
isPossible _ [] = False
isPossible (Pattern "") _ = True
isPossible (Pattern p) ts =
    any (\t -> isPossible (newPattern t) ts) $
        filter (`isPrefixOf` p) ts
  where
    newPattern t = Pattern (drop (length t) p)

{- | Part 2

Example:

>>> let example = ""
>>> part2 example
-}
part2 :: String -> Int
part2 = undefined

-- >>> let example = "r, wr, b, g, bwu, rb, gb, br\n\nbrwrr\nbggr\ngbbr\nrrbgbr\nubwu\nbwurrg\nbrgr\nbbrgwb"
-- >>> pInput example
-- (["r","wr","b","g","bwu","rb","gb","br"],[Pattern "brwrr",Pattern "bggr",Pattern "gbbr",Pattern "rrbgbr",Pattern "ubwu",Pattern "bwurrg",Pattern "brgr",Pattern "bbrgwb"])
pInput :: String -> ([String], [Pattern])
pInput s = (towels, patterns)
  where
    towels = map strip . splitOn "," . head . lines $ s
    patterns = map Pattern . drop 2 . lines $ s

solve :: IO ()
solve = do
    -- example <- readFile "./src/Year2024/data/day19-example.txt"
    -- print $ "Part1 example: " ++ show (part1 example)
    -- print $ "Part2 example: " ++ show (part2 example)

    content <- readFile "./src/Year2024/data/day19.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
