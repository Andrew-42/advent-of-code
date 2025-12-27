{-# LANGUAGE MultiParamTypeClasses #-}

module Year2025.Day10 (solve) where

import Data.List (intercalate, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Search (SearchProblem (..), bfs)
import System.Process (readProcess)
import Utils (indexed, itemAt, setItemAt)

{- | --- Day 10: Factory ---

Just across the hall, you find a large factory. Fortunately, the Elves here have plenty of time to
decorate. Unfortunately, it's because the factory machines are all offline, and none of the Elves
can figure out the initialization procedure.

The Elves do have the manual for the machines, but the section detailing the initialization
procedure was eaten by a Shiba Inu. All that remains of the manual are some indicator light
diagrams, button wiring schematics, and joltage requirements for each machine.

For example:

[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}

The manual describes one machine per line. Each line contains a single indicator light diagram in
[square brackets], one or more button wiring schematics in (parentheses), and joltage requirements
in {curly braces}.

To start a machine, its indicator lights must match those shown in the diagram, where . means off
and # means on. The machine has the number of indicator lights shown, but its indicator lights are
all initially off.

So, an indicator light diagram like [.##.] means that the machine has four indicator lights which
are initially off and that the goal is to simultaneously configure the first light to be off, the
second light to be on, the third to be on, and the fourth to be off.

You can toggle the state of indicator lights by pushing any of the listed buttons. Each button lists
which indicator lights it toggles, where 0 means the first light, 1 means the second light, and so
on. When you push a button, each listed indicator light either turns on (if it was off) or turns off
(if it was on). You have to push each button an integer number of times; there's no such thing as
"0.5 presses" (nor can you push a button a negative number of times).

So, a button wiring schematic like (0,3,4) means that each time you push that button, the first,
fourth, and fifth indicator lights would all toggle between on and off. If the indicator lights were
[#.....], pushing the button would change them to be [...##.] instead.

Because none of the machines are running, the joltage requirements are irrelevant and can be safely
ignored.

You can push each button as many times as you like. However, to save on time, you will need to
determine the fewest total presses required to correctly configure all indicator lights for all
machines in your list.

There are a few ways to correctly configure the first machine:

[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}

You could press the first three buttons once each, a total of 3 button presses.
You could press (1,3) once, (2,3) once, and (0,1) twice, a total of 4 button presses.
You could press all of the buttons except (1,3) once each, a total of 5 button presses.
However, the fewest button presses required is 2. One way to do this is by pressing the last two
buttons ((0,2) and (0,1)) once each.

The second machine can be configured with as few as 3 button presses:

[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}

One way to achieve this is by pressing the last three buttons ((0,4), (0,1,2), and (1,2,3,4)) once
each.

The third machine has a total of six indicator lights that need to be configured correctly:

[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}

The fewest presses required to correctly configure it is 2; one way to do this is by pressing
buttons (0,3,4) and (0,1,2,4,5) once each.

So, the fewest button presses required to correctly configure the indicator lights on all of the
machines is 2 + 3 + 2 = 7.

Analyze each machine's indicator light diagram and button wiring schematics. What is the fewest
button presses required to correctly configure the indicator lights on all of the machines?

Example:

>>> let example = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
>>> part1 example
7
-}
part1 :: T.Text -> Int
part1 = sum . map (getPresses . solveMachine) . pInput
  where
    solveMachine (Machine finalSwitch _ btns) = bfs sp
      where
        initSol = Solution 0 (allOff finalSwitch)
        sp = SearchProblem initSol btns step (isFinished finalSwitch)

{- | Part 2

All of the machines are starting to come online! Now, it's time to worry about the joltage
requirements.

Each machine needs to be configured to exactly the specified joltage levels to function properly.
Below the buttons on each machine is a big lever that you can use to switch the buttons from
configuring the indicator lights to increasing the joltage levels. (Ignore the indicator light
diagrams.)

The machines each have a set of numeric counters tracking its joltage levels, one counter per
joltage requirement. The counters are all initially set to zero.

So, joltage requirements like {3,5,4,7} mean that the machine has four counters which are initially
0 and that the goal is to simultaneously configure the first counter to be 3, the second counter to
be 5, the third to be 4, and the fourth to be 7.

The button wiring schematics are still relevant: in this new joltage configuration mode, each button
now indicates which counters it affects, where 0 means the first counter, 1 means the second
counter, and so on. When you push a button, each listed counter is increased by 1.

So, a button wiring schematic like (1,3) means that each time you push that button, the second and
fourth counters would each increase by 1. If the current joltage levels were {0,1,2,3}, pushing the
button would change them to be {0,2,2,4}.

You can push each button as many times as you like. However, your finger is getting sore from all
the button pushing, and so you will need to determine the fewest total presses required to correctly
configure each machine's joltage level counters to match the specified joltage requirements.

Consider again the example from before:

[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}

Configuring the first machine's counters requires a minimum of 10 button presses. One way to do this
is by pressing (3) once, (1,3) three times, (2,3) three times, (0,2) once, and (0,1) twice.

Configuring the second machine's counters requires a minimum of 12 button presses. One way to do
this is by pressing (0,2,3,4) twice, (2,3) five times, and (0,1,2) five times.

Configuring the third machine's counters requires a minimum of 11 button presses. One way to do this
is by pressing (0,1,2,3,4) five times, (0,1,2,4,5) five times, and (1,2) once.

So, the fewest button presses required to correctly configure the joltage level counters on all of
the machines is 10 + 12 + 11 = 33.

Analyze each machine's joltage requirements and button wiring schematics. What is the fewest button
presses required to correctly configure the joltage level counters on all of the machines?

Example:

>>> let example = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
>>> part2 example
33
-}
part2 :: T.Text -> IO Int
part2 text = do
    res <- mapM solveGlpk (pInput text)
    return $ sum res

solveGlpk :: Machine -> IO Int
solveGlpk m = do
    writeFile "example.lp" (createProblem m)
    _ <-
        readProcess
            "glpsol"
            ["--lp", "example.lp", "--tmlim", "60", "--mipgap", "0.0", "-o", "output.txt"]
            ""
    res <- readFile "output.txt"
    return $ pObjective res
  where
    pObjective txt = case words . head . dropWhile (not . isPrefixOf "Objective:") . lines $ txt of
        ["Objective:", "obj", "=", x, "(MINimum)"] -> read x
        l -> error $ "Invalid objective: " ++ show l

createProblem :: Machine -> String
createProblem (Machine _ (Joltage js) btns) = objective ++ constraints ++ bounds ++ end
  where
    vars = map (\i -> "x_" ++ show i) [1 .. length btns]
    objective = "Minimize\n obj: " ++ intercalate " + " vars ++ "\n\n"
    constraints = "Subject To\n" ++ (unlines . map (uncurry constraint) . indexed $ js) ++ "\n"
    bounds = "Bounds\n" ++ unlines (map (\v -> " " ++ v ++ " >= 0") vars) ++ "\n"
    end = "General\n " ++ unwords vars ++ "\n\nEnd"

    constraint i eq = " c" ++ show i ++ ": " ++ intercalate " + " constraintVars ++ " = " ++ show eq
      where
        constraintVars = map fst . filter (\(_, btn) -> contributes btn) $ zip vars btns
        contributes (Button xs) = i `elem` xs

-- >>> pInput example
-- [Machine (Switch {getSwitch = [OFF,ON,ON,OFF]}) (Joltage {getJoltage = [3,5,4,7]}) [Button {getIndices = [3]},Button {getIndices = [1,3]},Button {getIndices = [2]},Button {getIndices = [2,3]},Button {getIndices = [0,2]},Button {getIndices = [0,1]}],Machine (Switch {getSwitch = [OFF,OFF,OFF,ON,OFF]}) (Joltage {getJoltage = [7,5,12,7,2]}) [Button {getIndices = [0,2,3,4]},Button {getIndices = [2,3]},Button {getIndices = [0,4]},Button {getIndices = [0,1,2]},Button {getIndices = [1,2,3,4]}],Machine (Switch {getSwitch = [OFF,ON,ON,ON,OFF,ON]}) (Joltage {getJoltage = [10,11,11,5,10,5]}) [Button {getIndices = [0,1,2,3,4]},Button {getIndices = [0,3,4]},Button {getIndices = [0,1,2,4,5]},Button {getIndices = [1,2]}]]
pInput :: T.Text -> [Machine]
pInput text = zipWith3 Machine switches joltages buttons
  where
    ls = map T.words . T.lines $ text
    switches = map (Switch . map pState . init . tail . T.unpack . head) ls
      where
        pState c = case c of
            '.' -> OFF
            '#' -> ON
            _ -> error "Invalid switch state."
    buttons = map (map pButton . init . tail) ls
      where
        pButton = Button . map read . splitOn "," . init . tail . T.unpack
    joltages = map (pJoltage . last) ls
      where
        pJoltage = Joltage . map read . splitOn "," . init . tail . T.unpack

data Machine = Machine Switch Joltage [Button] deriving (Show)

data Solution = Solution {getPresses :: Int, getSwitch :: Switch} deriving (Eq, Ord, Show)

-- >>> isFinished (Switch [OFF,ON,ON,OFF]) (Solution 2 (Switch [OFF,ON,ON,OFF]))
-- True
isFinished :: Switch -> Solution -> Bool
isFinished solSwitch (Solution _ switch') = solSwitch == switch'

step :: Solution -> [Button] -> ([Solution], [Button])
step (Solution n switch') btns = (map (Solution (n + 1) . pushSwitch switch') btns, btns)

data SwitchState = ON | OFF deriving (Eq, Ord, Show)

turn :: SwitchState -> SwitchState
turn ON = OFF
turn OFF = ON

newtype Switch = Switch [SwitchState] deriving (Eq, Ord, Show)

allOff :: Switch -> Switch
allOff (Switch switchStates) = Switch $ map (const OFF) switchStates

newtype Joltage = Joltage [Int] deriving (Eq, Ord, Show)

newtype Button = Button [Int] deriving (Eq, Show)

-- >>> pushButton (Switch [OFF, OFF, OFF, OFF]) (Button [1, 2])
-- Switch {getSwitch = [OFF,ON,ON,OFF]}
pushSwitch :: Switch -> Button -> Switch
pushSwitch (Switch switchStates) (Button indices) = Switch $ foldl turn' switchStates indices
  where
    turn' ss i = fromJust $ itemAt i ss >>= \s -> setItemAt i (turn s) ss

solve :: IO ()
solve = do
    content <- TIO.readFile "./src/Year2025/data/day10.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    solution <- part2 content
    print $ "Part2 solution: " ++ show solution
