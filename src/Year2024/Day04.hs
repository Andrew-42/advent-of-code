module Year2024.Day04 (solve) where

import Data.List.Split (splitOn)

{- | Part 1

"Looks like the Chief's not here. Next!" One of The Historians pulls out a
device and pushes the only button on it. After a brief flash, you recognize the
interior of the Ceres monitoring station!

As the search for the Chief continues, a small Elf who lives on the station tugs
on your shirt; she'd like to know if you could help her with her word search
(your puzzle input). She only has to find one word: XMAS.

This word search allows words to be horizontal, vertical, diagonal, written
backwards, or even overlapping other words. It's a little unusual, though, as
you don't merely need to find one instance of XMAS - you need to find all of
them. Here are a few ways XMAS might appear, where irrelevant characters have
been replaced with .:

..X...
.SAMX.
.A..A.
XMAS.S
.X....

The actual word search will be full of letters instead. For example:

MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX

In this word search, XMAS occurs a total of 18 times; here's the same word
search again, but where letters not involved in any XMAS have been replaced
with .:

....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX

Take a look at the little Elf's word search. How many times does XMAS appear?

Example:

>>> let example = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"
>>> part1 example
18
-}
part1 :: String -> Int
part1 = sum . sequenceA [countLines, countColumns, countDiagonals] . lines

-- >>> transpose ["abc", "abc", "abc"]
-- ["aaa","bbb","ccc"]
transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose xs = map head xs : transpose (map tail xs)

countLines :: [String] -> Int
countLines = sum . map ((+) <$> countXmas <*> countXmas . reverse)

countColumns :: [String] -> Int
countColumns = countLines . transpose

countDiagonals :: [String] -> Int
countDiagonals = (+) <$> sum . countDiagonalR <*> sum . countDiagonalL

countDiagonalR :: [String] -> [Int]
countDiagonalR [] = []
countDiagonalR [_] = []
countDiagonalR [_, _] = []
countDiagonalR [_, _, _] = []
countDiagonalR xs@(a : b : c : d : _) = diagCount : countDiagonalR (tail xs)
 where
  l1 = drop 3 a
  l2 = dropLast 1 . drop 2 $ b
  l3 = dropLast 2 . drop 1 $ c
  l4 = dropLast 3 d

  diagCount = countColumns [l1, l2, l3, l4]

countDiagonalL :: [String] -> [Int]
countDiagonalL [] = []
countDiagonalL [_] = []
countDiagonalL [_, _] = []
countDiagonalL [_, _, _] = []
countDiagonalL xs@(a : b : c : d : _) = diagCount : countDiagonalL (tail xs)
 where
  l1 = dropLast 3 a
  l2 = dropLast 2 . drop 1 $ b
  l3 = dropLast 1 . drop 2 $ c
  l4 = drop 3 d

  diagCount = countColumns [l1, l2, l3, l4]

dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse

countXmas :: String -> Int
countXmas l = length (splitOn "XMAS" l) - 1

{- | Part 2

The Elf looks quizzically at you. Did you misunderstand the assignment?

Looking for the instructions, you flip over the word search to find that this
isn't actually an XMAS puzzle; it's an X-MAS puzzle in which you're supposed to
find two MAS in the shape of an X. One way to achieve that is like this:

M.S
.A.
M.S

Irrelevant characters have again been replaced with . in the above diagram.
Within the X, each MAS can be written forwards or backwards.

Here's the same example from before, but this time all of the X-MASes have been
kept instead:

.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........

In this example, an X-MAS appears 9 times.

Flip the word search from the instructions back over to the word search side and
try again. How many times does an X-MAS appear?

Example:

>>> let example = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"
>>> part2 example
9
-}
part2 :: String -> Int
part2 = sum . rollWindow . lines

rollWindow :: [String] -> [Int]
rollWindow [] = []
rollWindow [_] = []
rollWindow [_, _] = []
rollWindow xs@(a : b : c : _) = length (filter isMas triples) : rollWindow (tail xs)
 where
  triples = zip3 (windows a) (windows b) (windows c)
  windows s = zip3 s (drop 1 s) (drop 2 s)

type Triple = (Char, Char, Char)

isMas :: (Triple, Triple, Triple) -> Bool
isMas ((a1, _, c1), (_, b2, _), (a3, _, c3)) = isValid s1 && isValid s2
 where
  s1 = [a1, b2, c3]
  s2 = [c1, b2, a3]

  isValid s = s == "MAS" || s == "SAM"

solve :: IO ()
solve = do
  content <- readFile "./src/Year2024/data/day04.txt"
  print $ "Part1 solution: " ++ show (part1 content)
  print $ "Part2 solution: " ++ show (part2 content)
