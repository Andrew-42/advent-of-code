module Year2024.Day03 (solve) where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isDigit)
import Data.List (stripPrefix, tails)
import Data.Maybe (mapMaybe)

{- | Part 1

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
