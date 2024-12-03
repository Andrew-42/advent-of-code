module Year2024.Day03 (solve) where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isDigit)
import Data.List (stripPrefix, tails)
import Data.Maybe (mapMaybe)

{- | Part 1

"Our computers are having issues, so I have no idea if we have any Chief
Historians in stock! You're welcome to check the warehouse, though," says the
mildly flustered shopkeeper at the North Pole Toboggan Rental Shop. The
Historians head out to take a look.

The shopkeeper turns to you. "Any chance you can see why our computers are
having issues again?"

The computer appears to be trying to run a program, but its memory (your puzzle
input) is corrupted. All of the instructions have been jumbled up!

It seems like the goal of the program is just to multiply some numbers. It does
that with instructions like mul(X,Y), where X and Y are each 1-3 digit numbers.
For instance, mul(44,46) multiplies 44 by 46 to get a result of 2024. Similarly,
mul(123,4) would multiply 123 by 4.

However, because the program's memory has been corrupted, there are also many
invalid characters that should be ignored, even if they look like part of a mul
instruction. Sequences like mul(4*, mul(6,9!, ?(12,34), or mul ( 2 , 4 ) do
nothing.

For example, consider the following section of corrupted memory:

xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))

Only the four highlighted sections are real mul instructions. Adding up the
result of each instruction produces 161 (2*4 + 5*5 + 11*8 + 8*5).

Scan the corrupted memory for uncorrupted mul instructions. What do you get if
you add up all of the results of the multiplications?


Example should be 161:

>>> let example = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
>>> part1 example
161
-}
part1 :: String -> Int
part1 = sum . map multiply . mapMaybe parseInstruction . tails

multiply :: Instruction -> Int
multiply (Mul a b) = a * b
multiply _ = 0

{- | Part 2

As you scan through the corrupted memory, you notice that some of the
conditional statements are also still intact. If you handle some of the
uncorrupted conditional statements in the program, you might be able to get an
even more accurate result.

There are two new instructions you'll need to handle:

The do() instruction enables future mul instructions.
The don't() instruction disables future mul instructions.
Only the most recent do() or don't() instruction applies. At the beginning of
the program, mul instructions are enabled.

For example:

xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))

This corrupted memory is similar to the example from before, but this time the
mul(5,5) and mul(11,8) instructions are disabled because there is a don't()
instruction before them. The other mul instructions function normally, including
the one at the end that gets re-enabled by a do() instruction.

This time, the sum of the results is 48 (2*4 + 8*5).

Handle the new instructions; what do you get if you add up all of the results of
just the enabled multiplications?

Example should be 48:

>>> let example = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
>>> part2 example
48
-}
part2 :: String -> Int
part2 = snd . evalEnabled (True, 0) . mapMaybe parseInstruction . tails

data Instruction = Mul Int Int | Do | Dont deriving (Show)

-- >>> parseMul "mul(12,436)"
-- Just (Mul 12 436)
parseMul :: String -> Maybe Instruction
parseMul s = do
    s0 <- stripPrefix "mul(" s
    n1 <- takeNum s0
    s1 <- stripPrefix (n1 ++ ",") s0
    n2 <- takeNum s1
    _ <- stripPrefix (n2 ++ ")") s1
    return $ Mul (read n1) (read n2)

takeNum :: String -> Maybe String
takeNum s = if inBounds num then Just num else Nothing
  where
    num :: String
    num = takeWhile isDigit s

    inBounds :: String -> Bool
    inBounds ds = length ds `elem` [1 .. 3]

-- >>> parseDo "do()dbrakadabra"
-- >>> parseDo "don't()dbrakadabra"
-- Just Do
-- Nothing
parseDo :: String -> Maybe Instruction
parseDo s = Do <$ stripPrefix "do()" s

-- >>> parseDont "do()dbrakadabra"
-- >>> parseDont "don't()dbrakadabra"
-- Nothing
-- Just Dont
parseDont :: String -> Maybe Instruction
parseDont s = Dont <$ stripPrefix "don't()" s

-- >>> let example = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
-- >>> mapMaybe parseInstruction $ tails example
-- [Mul 2 4,Dont,Mul 5 5,Mul 11 8,Do,Mul 8 5]
parseInstruction :: String -> Maybe Instruction
parseInstruction s = parseMul s <|> parseDo s <|> parseDont s

-- >>> let example = [Mul 2 4,Dont,Mul 5 5,Mul 11 8,Do,Mul 8 5]
-- >>> evalEnabled (True, 0) example
-- (True,48)
evalEnabled :: (Bool, Int) -> [Instruction] -> (Bool, Int)
evalEnabled acc [] = acc
evalEnabled (enabled, count) (i : is) = case i of
    Do -> evalEnabled (True, count) is
    Dont -> evalEnabled (False, count) is
    Mul a b
        | enabled -> evalEnabled (enabled, count + (a * b)) is
        | otherwise -> evalEnabled (enabled, count) is

solve :: IO ()
solve = do
    content <- readFile "./src/Year2024/data/day03.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
