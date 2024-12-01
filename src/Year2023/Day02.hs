module Year2023.Day02 (solve) where

import Data.List.Split (splitOn)
import Utils (strip)

{- | Part 1

You're launched high into the atmosphere! The apex of your trajectory just
barely reaches the surface of a large island floating in the sky. You gently
land in a fluffy pile of leaves. It's quite cold, but you don't see much snow.
An Elf runs over to greet you.

The Elf explains that you've arrived at Snow Island and apologizes for the lack
of snow. He'll be happy to explain the situation, but it's a bit of a walk, so
you have some time. They don't get many visitors up here; would you like to play
a game in the meantime?

As you walk, the Elf shows you a small bag and some cubes which are either red,
green, or blue. Each time you play this game, he will hide a secret number of
cubes of each color in the bag, and your goal is to figure out information about
the number of cubes.

To get information, once a bag has been loaded with cubes, the Elf will reach
into the bag, grab a handful of random cubes, show them to you, and then put
them back in the bag. He'll do this a few times per game.

You play several games and record the information from each game (your puzzle
input). Each game is listed with its ID number (like the 11 in Game 11: ...)
followed by a semicolon-separated list of subsets of cubes that were revealed
from the bag (like 3 red, 5 green, 4 blue).

For example, the record of a few games might look like this:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

In game 1, three sets of cubes are revealed from the bag (and then put back
again). The first set is 3 blue cubes and 4 red cubes; the second set is 1 red
cube, 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes.

The Elf would first like to know which games would have been possible if the bag
contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

In the example above, games 1, 2, and 5 would have been possible if the bag had
been loaded with that configuration. However, game 3 would have been impossible
because at one point the Elf showed you 20 red cubes at once; similarly, game 4
would also have been impossible because the Elf showed you 15 blue cubes at
once. If you add up the IDs of the games that would have been possible, you
get 8.

Determine which games would have been possible if the bag had been loaded with
only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the IDs
of those games?

Example should be 8:

>>> let example = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
>>> part1 example
8
-}
part1 :: String -> Int
part1 =
    sum
        . map gameId
        . filter gamePossible
        . map parseGame
        . lines

gamePossible :: Game -> Bool
gamePossible (Game _ rounds) = all roundPossible rounds
  where
    roundPossible (Round balls) = all ballPossible balls

    ballPossible b = case b of
        Red x -> x <= 12
        Green x -> x <= 13
        Blue x -> x <= 14

{- | Part 2

The Elf says they've stopped producing snow because they aren't getting any
water! He isn't sure why the water stopped; however, he can show you how to get
to the water source to check it out for yourself. It's just up ahead!

As you continue your walk, the Elf poses a second question: in each game you
played, what is the fewest number of cubes of each color that could have been in
the bag to make the game possible?

Again consider the example games from earlier:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

In game 1, the game could have been played with as few as 4 red, 2 green, and 6
blue cubes. If any color had even one fewer cube, the game would have been
impossible.

Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
Game 4 required at least 14 red, 3 green, and 15 blue cubes.
Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.

The power of a set of cubes is equal to the numbers of red, green, and blue
cubes multiplied together. The power of the minimum set of cubes in game 1 is
48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding up these
five powers produces the sum 2286.

For each game, find the minimum set of cubes that must have been present. What
is the sum of the power of these sets?

Example should be 2286:

>>> let example = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
>>> part2 example
2286
-}
part2 :: String -> Int
part2 = sum . map (minGamePossible . parseGame) . lines

minGamePossible :: Game -> Int
minGamePossible (Game _ rounds) = minRed * minGreen * minBlue
  where
    minRed = maximum . concatMap (gameBalls isRed) $ rounds
    minGreen = maximum . concatMap (gameBalls isGreen) $ rounds
    minBlue = maximum . concatMap (gameBalls isBlue) $ rounds

    gameBalls p = map ballToInt . filter p . roundBalls

    isRed b = case b of
        Red _ -> True
        _ -> False

    isGreen b = case b of
        Green _ -> True
        _ -> False

    isBlue b = case b of
        Blue _ -> True
        _ -> False

    ballToInt b = case b of
        Red x -> x
        Green x -> x
        Blue x -> x

-- >>> let example = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
-- >>> parseGame example
-- Game 1 [Round [Blue 3,Red 4],Round [Red 1,Green 2,Blue 6],Round [Green 2]]
parseGame :: [Char] -> Game
parseGame s = Game getId getRounds
  where
    (g, rounds) = (\ws -> (head ws, splitOn ";" $ last ws)) . splitOn ":" $ s

    getId = read . last . words $ g

    getRounds = map (Round . map (parseBall . strip) . splitOn ",") rounds

    parseBall b = case words b of
        [x, "red"] -> Red $ read x
        [x, "green"] -> Green $ read x
        [x, "blue"] -> Blue $ read x
        _ -> error "invalid input"

data Game = Game {gameId :: Int, gameRounds :: [Round]} deriving (Show)

newtype Round = Round {roundBalls :: [Ball]} deriving (Show)

data Ball = Red Int | Green Int | Blue Int deriving (Show, Eq, Ord)

solve :: IO ()
solve = do
    content <- readFile "./src/Year2023/data/day02.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
