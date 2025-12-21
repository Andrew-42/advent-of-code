module Year2025.Day10 (solve) where

import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace, traceShow)
import Search (SearchProblem (SearchProblem), bfs)
import Utils (itemAt, setItemAt)

example :: T.Text
example =
    T.unlines
        [ "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
        , "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
        , "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
        ]

{- | Part 1

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

Example:

>>> let example = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
>>> part2 example
33
-}
part2 :: T.Text -> Int
part2 = getPresses' . solveMachine . head . pInput
  where
    -- part2 = sum . map (getPresses' . solveMachine) . pInput

    solveMachine (Machine _ finalJoltage btns) = bfs sp
      where
        initSol = Solution' (sum finalJoltage.getJoltage) 0 (allZero finalJoltage)
        sp = SearchProblem initSol btns (step' finalJoltage) (isFinished' finalJoltage)

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

data SolutionJ = Solution' {getPriority :: Int, getPresses' :: Int, getJoltage :: Joltage}
    deriving (Eq, Ord, Show)

-- >>> isFinished (Switch [OFF,ON,ON,OFF]) (Solution 2 (Switch [OFF,ON,ON,OFF]))
-- True
isFinished :: Switch -> Solution -> Bool
isFinished solSwitch (Solution _ switch') = solSwitch == switch'

isFinished' :: Joltage -> SolutionJ -> Bool
isFinished' solJoltage (Solution' _ _ js) = solJoltage == js

deadState :: Joltage -> SolutionJ -> Bool
deadState (Joltage js) (Solution' _ _ (Joltage sjs)) = any (uncurry (<)) $ zip js sjs

step :: Solution -> [Button] -> (S.Set Solution, [Button])
step (Solution n switch') btns =
    (S.fromList $ map (Solution (n + 1) . pushSwitch switch') btns, btns)

step' :: Joltage -> SolutionJ -> [Button] -> (S.Set SolutionJ, [Button])
step' targetJoltage sol@(Solution' p n js) btns = traceShow sol (nJoltages btns, btns)
  where
    nJoltages =
        S.fromList
            . filter (not . deadState targetJoltage)
            . map (\b -> Solution' (p - length (getIndices b)) (n + 1) . pushJoltage js $ b)

data SwitchState = ON | OFF deriving (Eq, Ord, Show)

turn :: SwitchState -> SwitchState
turn ON = OFF
turn OFF = ON

newtype Switch = Switch {getSwitch :: [SwitchState]} deriving (Eq, Ord, Show)

allOff :: Switch -> Switch
allOff (Switch switchStates) = Switch $ map (const OFF) switchStates

newtype Joltage = Joltage {getJoltage :: [Int]} deriving (Eq, Ord, Show)

allZero :: Joltage -> Joltage
allZero (Joltage js) = Joltage $ map (const 0) js

newtype Button = Button {getIndices :: [Int]} deriving (Eq, Show)

-- >>> pushButton (Switch [OFF, OFF, OFF, OFF]) (Button [1, 2])
-- Switch {getSwitch = [OFF,ON,ON,OFF]}
pushSwitch :: Switch -> Button -> Switch
pushSwitch (Switch switchStates) (Button indices) = Switch $ foldl turn' switchStates indices
  where
    turn' ss i = fromJust $ itemAt i ss >>= \s -> setItemAt i (turn s) ss

pushJoltage :: Joltage -> Button -> Joltage
pushJoltage (Joltage js) (Button indices) = Joltage $ foldl inc js indices
  where
    inc ss i = fromJust $ itemAt i ss >>= \n -> setItemAt i (n + 1) ss

solve :: IO ()
solve = do
    content <- TIO.readFile "./src/Year2025/data/day10.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
