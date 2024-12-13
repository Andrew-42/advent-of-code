module Year2024.Day13 (solve) where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Utils (dropLast)

{- | Part 1

Example should be 480:

Next up: the lobby of a resort on a tropical island. The Historians take a
moment to admire the hexagonal floor tiles before spreading out.

Fortunately, it looks like the resort has a new arcade! Maybe you can win some
prizes from the claw machines?

The claw machines here are a little unusual. Instead of a joystick or
directional buttons to control the claw, these machines have two buttons labeled
A and B. Worse, you can't just put in a token and play; it costs 3 tokens to
push the A button and 1 token to push the B button.

With a little experimentation, you figure out that each machine's buttons are
configured to move the claw a specific amount to the right (along the X axis)
and a specific amount forward (along the Y axis) each time that button is
pressed.

Each machine contains one prize; to win the prize, the claw must be positioned
exactly above the prize on both the X and Y axes.

You wonder: what is the smallest number of tokens you would have to spend to win
as many prizes as possible? You assemble a list of every machine's button
behavior and prize location (your puzzle input). For example:

Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279

This list describes the button configuration and prize location of four
different claw machines.

For now, consider just the first claw machine in the list:

- Pushing the machine's A button would move the claw 94 units along the X axis
  and 34 units along the Y axis.
- Pushing the B button would move the claw 22 units along the X axis and 67
  units along the Y axis.
- The prize is located at X=8400, Y=5400; this means that from the claw's
  initial position, it would need to move exactly 8400 units along the X axis
  and exactly 5400 units along the Y axis to be perfectly aligned with the prize
  in this machine.
- The cheapest way to win the prize is by pushing the A button 80 times and the
  B button 40 times. This would line up the claw along the X axis
  (because 80*94 + 40*22 = 8400) and along the Y axis
  (because 80*34 + 40*67 = 5400). Doing this would cost 80*3 tokens for the A
  presses and 40*1 for the B presses, a total of 280 tokens.

For the second and fourth claw machines, there is no combination of A and B
presses that will ever win a prize.

For the third claw machine, the cheapest way to win the prize is by pushing the
A button 38 times and the B button 86 times. Doing this would cost a total of
200 tokens.

So, the most prizes you could possibly win is two; the minimum tokens you would
have to spend to win all (two) prizes is 480.

You estimate that each button would need to be pressed no more than 100 times to
win a prize. How else would someone be expected to play?

Figure out how to win as many prizes as possible. What is the fewest tokens you
would have to spend to win all possible prizes?

>>> let example = "Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400\n\nButton A: X+26, Y+66\nButton B: X+67, Y+21\nPrize: X=12748, Y=12176\n\nButton A: X+17, Y+86\nButton B: X+84, Y+37\nPrize: X=7870, Y=6450\n\nButton A: X+69, Y+23\nButton B: X+27, Y+71\nPrize: X=18641, Y=10279"
>>> part1 example
480
-}
part1 :: String -> Int
part1 =
    sum
        . map findSolution
        . mapMaybe (parseMachine . unlines)
        . splitOn [""]
        . lines

data Machine = Machine Button Button Solution deriving (Show, Eq)
data Button = Button Int Int deriving (Show, Eq)
data Solution = Solution Int Int deriving (Show, Eq)

-- >>> parseMachine "Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400\n"
-- Just (Machine (Button 94 34) (Button 22 67) (Prize 8400 5400))
parseMachine :: String -> Maybe Machine
parseMachine s = do
    [bA, bB, p] <- Just $ lines s
    buttonA <- parseButton bA
    buttonB <- parseButton bB
    prize <- parsePrize p
    return $ Machine buttonA buttonB prize

-- >>> parseButton "Button A: X+94, Y+34"
-- Just (Button 94 34)
parseButton :: String -> Maybe Button
parseButton s = do
    ["Button", _, x, y] <- Just $ words s
    ["X", valX] <- Just $ splitOn "+" (dropLast 1 x)
    ["Y", valY] <- Just $ splitOn "+" y
    return $ Button (read valX) (read valY)

-- >>> parsePrize "Prize: X=8400, Y=5400"
-- Just (Prize 8400 5400)
parsePrize :: String -> Maybe Solution
parsePrize s = do
    ["Prize:", x, y] <- Just $ words s
    ["X", valX] <- Just $ splitOn "=" (dropLast 1 x)
    ["Y", valY] <- Just $ splitOn "=" y
    return $ Solution (read valX) (read valY)

-- >>> isSolution (Solution 80 40) (Machine (Button 94 34) (Button 22 67) (Solution 8400 5400))
-- True
isSolution :: Solution -> Machine -> Bool
isSolution (Solution x y) (Machine bA bB p) =
    addSolutions (mulButton x bA) (mulButton y bB) == p

toTokens :: Solution -> Int
toTokens (Solution x y) = 3 * x + y

mulButton :: Int -> Button -> Solution
mulButton n (Button x y) = Solution (n * x) (n * y)

addSolutions :: Solution -> Solution -> Solution
addSolutions (Solution x1 y1) (Solution x2 y2) = Solution (x1 + x2) (y1 + y2)

-- >>> findSolution (Machine (Button 94 34) (Button 22 67) (Solution 8400 5400))
-- 280
findSolution :: Machine -> Int
findSolution m = case filter (`isSolution` m) possibleSolutions of
    [] -> 0
    (s : _) -> toTokens s
  where
    possibleSolutions = [Solution x y | x <- [0 .. 100], y <- [0 .. 100]]

{- | Part 2

As you go to win the first prize, you discover that the claw is nowhere near
where you expected it would be. Due to a unit conversion error in your
measurements, the position of every prize is actually 10000000000000 higher on
both the X and Y axis!

Add 10000000000000 to the X and Y position of every prize. After making this
change, the example above would now look like this:

Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=10000000008400, Y=10000000005400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=10000000012748, Y=10000000012176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=10000000007870, Y=10000000006450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=10000000018641, Y=10000000010279

Now, it is only possible to win a prize on the second and fourth claw machines.
Unfortunately, it will take many more than 100 presses to do so.

Using the corrected prize coordinates, figure out how to win as many prizes as
possible. What is the fewest tokens you would have to spend to win all possible
prizes?

Example should be 875318608908:

>>> let example = "Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400\n\nButton A: X+26, Y+66\nButton B: X+67, Y+21\nPrize: X=12748, Y=12176\n\nButton A: X+17, Y+86\nButton B: X+84, Y+37\nPrize: X=7870, Y=6450\n\nButton A: X+69, Y+23\nButton B: X+27, Y+71\nPrize: X=18641, Y=10279"
>>> part2 example
875318608908
-}
part2 :: String -> Int
part2 =
    sum
        . map findSolution'
        . mapMaybe (parseMachine' . unlines)
        . splitOn [""]
        . lines

addSolution :: Int -> Solution -> Solution
addSolution n (Solution x y) = Solution (n + x) (n + y)

-- >>> parseMachine' "Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400\n"
-- Just (Machine (Button 94 34) (Button 22 67) (Solution 10000000008400 10000000005400))
parseMachine' :: String -> Maybe Machine
parseMachine' s = do
    [bA, bB, p] <- Just $ lines s
    buttonA <- parseButton bA
    buttonB <- parseButton bB
    prize <- parsePrize p
    return $ Machine buttonA buttonB (addSolution 10000000000000 prize)

-- >>> findSolution' (Machine (Button 94 34) (Button 22 67) (Solution 8400 5400))
-- 280
findSolution' :: Machine -> Int
findSolution' m@(Machine (Button a1 a2) (Button b1 b2) (Solution c1 c2))
    | (b2 * a1) /= (b1 * a2) =
        if isSolution sol m then toTokens sol else 0
    | c2 * a1 == c1 * a2 = 0
    | otherwise = 0
  where
    n2 = (c2 * a1 - c1 * a2) `div` (b2 * a1 - b1 * a2)
    n1 = (c1 - n2 * b1) `div` a1
    sol = Solution n1 n2

solve :: IO ()
solve = do
    content <- readFile "./src/Year2024/data/day13.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
