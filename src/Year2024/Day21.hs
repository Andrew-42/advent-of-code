module Year2024.Day21 (solve) where

import Data.Maybe (mapMaybe)
import Utils (dropLast, zipWithNext)

{- | Part 1

Example:

>>> let example = "029A\n980A\n179A\n456A\n379A"
>>> let codes = "879A\n508A\n463A\n593A\n189A"
>>> part1 example
>>> part1 codes
126384
198336
-}
part1 :: String -> Int
part1 = sum . map complexCode . lines
  where
    complexCode s' = pNum s' * length (processCode s')

-- >>> processCode "029A"
-- "v<<A>A>^Av<<A>>^AAvAA<^A>A<vA>^AAv<<A>^A>AvA^A<vA<AA>>^AvAA<^A>A<vA>^A<A>A<vA<AA>>^AvAA<^A>AAv<<A>A>^AvA^A<A>Av<<A>>^AvA^A<vA<AA>>^AvA^AvA<^A>AAA<vA>^Av<<A>^A>AvA^A"
processCode :: String -> String
processCode =
    processDirSequence
        . parseDirCode
        . processDirSequence
        . parseDirCode
        . processNumSequence
        . parseNumCode

data NumKey = K0 | K1 | K2 | K3 | K4 | K5 | K6 | K7 | K8 | K9 | KA
    deriving (Show, Eq)

-- >>> processNumSequence [KA,K1,K8,K9,KA]
-- "^<<A^^>A>AvvvA"
processNumSequence :: [NumKey] -> String
processNumSequence = concatMap (uncurry numKeyPad) . zipWithNext

numKeyPad :: NumKey -> NumKey -> String
numKeyPad k1 k2 = case k1 of
    K0 -> case k2 of
        K0 -> "A"
        K1 -> "^<A"
        K2 -> "^A"
        K3 -> "^>A"
        K4 -> "^^<A"
        K5 -> "^^A"
        K6 -> "^^>A"
        K7 -> "^^^<A"
        K8 -> "^^^A"
        K9 -> "^^^>A"
        KA -> ">A"
    K1 -> case k2 of
        K0 -> ">vA"
        K1 -> "A"
        K2 -> ">A"
        K3 -> ">>A"
        K4 -> "^A"
        K5 -> "^>A"
        K6 -> "^>>A"
        K7 -> "^^A"
        K8 -> "^^>A"
        K9 -> "^^>>A"
        KA -> ">>vA"
    K2 -> case k2 of
        K0 -> "vA"
        K1 -> "<A"
        K2 -> "A"
        K3 -> ">A"
        K4 -> "<^A"
        K5 -> "^A"
        K6 -> "^>A"
        K7 -> "<^^A"
        K8 -> "^^A"
        K9 -> "^^>A"
        KA -> ">vA"
    K3 -> case k2 of
        K0 -> "<vA"
        K1 -> "<<A"
        K2 -> "<A"
        K3 -> "A"
        K4 -> "<<^A"
        K5 -> "<^A"
        K6 -> "^A"
        K7 -> "<<^^A"
        K8 -> "<^^A"
        K9 -> "^^A"
        KA -> "vA"
    K4 -> case k2 of
        K0 -> ">vvA"
        K1 -> "vA"
        K2 -> "v>A"
        K3 -> "v>>A"
        K4 -> "A"
        K5 -> ">A"
        K6 -> ">>A"
        K7 -> "^A"
        K8 -> "^>A"
        K9 -> "^>>A"
        KA -> ">>vvA"
    K5 -> case k2 of
        K0 -> "vvA"
        K1 -> "<vA"
        K2 -> "vA"
        K3 -> "v>A"
        K4 -> "<A"
        K5 -> "A"
        K6 -> ">A"
        K7 -> "<^A"
        K8 -> "^A"
        K9 -> "^>A"
        KA -> ">vvA"
    K6 -> case k2 of
        K0 -> "<vvA"
        K1 -> "<<vA"
        K2 -> "<vA"
        K3 -> "vA"
        K4 -> "<<A"
        K5 -> "<A"
        K6 -> "A"
        K7 -> "<<^A"
        K8 -> "<^A"
        K9 -> "^A"
        KA -> "vvA"
    K7 -> case k2 of
        K0 -> ">vvvA"
        K1 -> "vvA"
        K2 -> "vv>A"
        K3 -> "vv>>A"
        K4 -> "vA"
        K5 -> "v>A"
        K6 -> "v>>A"
        K7 -> "A"
        K8 -> ">A"
        K9 -> ">>A"
        KA -> ">>vvvA"
    K8 -> case k2 of
        K0 -> "vvvA"
        K1 -> "<vvA"
        K2 -> "vvA"
        K3 -> "vv>A"
        K4 -> "<vA"
        K5 -> "vA"
        K6 -> "v>A"
        K7 -> "<A"
        K8 -> "A"
        K9 -> ">A"
        KA -> ">vvvA"
    K9 -> case k2 of
        K0 -> "<vvvA"
        K1 -> "<<vvA"
        K2 -> "<vvA"
        K3 -> "vvA"
        K4 -> "<<vA"
        K5 -> "<vA"
        K6 -> "vA"
        K7 -> "<<A"
        K8 -> "<A"
        K9 -> "A"
        KA -> "vvvA"
    KA -> case k2 of
        K0 -> "<A"
        K1 -> "^<<A"
        K2 -> "^<A"
        K3 -> "^A"
        K4 -> "^^<<A"
        K5 -> "^^<A"
        K6 -> "^^A"
        K7 -> "^^^<<A"
        K8 -> "^^^<A"
        K9 -> "^^^A"
        KA -> "A"

data DirKey = DU | DD | DL | DR | DA deriving (Show, Eq)

-- >>> processDirSequence [DA,DL,DA,DU,DA,DU,DU,DR,DA,DD,DD,DD,DA]
-- "v<<A>>^A<A>A<AAv>A^A<vAAA>^A"
processDirSequence :: [DirKey] -> String
processDirSequence = concatMap (uncurry dirKeyPad) . zipWithNext

dirKeyPad :: DirKey -> DirKey -> String
dirKeyPad k1 k2 = case k1 of
    DU -> case k2 of
        DU -> "A"
        DD -> "vA"
        DL -> "v<A"
        DR -> "v>A"
        DA -> ">A"
    DD -> case k2 of
        DU -> "^A"
        DD -> "A"
        DL -> "<A"
        DR -> ">A"
        DA -> ">^A"
    DL -> case k2 of
        DU -> ">^A"
        DD -> ">A"
        DL -> "A"
        DR -> ">>A"
        DA -> ">>^A"
    DR -> case k2 of
        DU -> "<^A"
        DD -> "<A"
        DL -> "<<A"
        DR -> "A"
        DA -> "^A"
    DA -> case k2 of
        DU -> "<A"
        DD -> "v<A"
        DL -> "v<<A"
        DR -> "vA"
        DA -> "A"

{- | Part 2

Example:

>>> let example = ""
>>> part2 example
-}
part2 :: String -> Int
part2 = undefined

pInput :: String -> a
pInput = undefined

-- >>> pNum "029A"
-- 29
pNum :: String -> Int
pNum = read . dropLast 1 . dropWhile (== '0')

-- >>> parseNumCode "029A"
-- [K0,K2,K9,KA]
parseNumCode :: String -> [NumKey]
parseNumCode s = KA : mapMaybe pNumKey s
  where
    pNumKey c = case c of
        '0' -> Just K0
        '1' -> Just K1
        '2' -> Just K2
        '3' -> Just K3
        '4' -> Just K4
        '5' -> Just K5
        '6' -> Just K6
        '7' -> Just K7
        '8' -> Just K8
        '9' -> Just K9
        'A' -> Just KA
        _ -> Nothing

-- >>> parseDirCode "<A^A^^>AvvvA"
-- [DA,DL,DA,DU,DA,DU,DU,DR,DA,DD,DD,DD,DA]
parseDirCode :: String -> [DirKey]
parseDirCode s = DA : mapMaybe pDirKey s
  where
    pDirKey c = case c of
        '^' -> Just DU
        'v' -> Just DD
        '<' -> Just DL
        '>' -> Just DR
        'A' -> Just DA
        _ -> Nothing

solve :: IO ()
solve = do
    -- example <- readFile "./src/Year2024/data/day21-example.txt"
    -- print $ "Part1 example: " ++ show (part1 example)
    -- print $ "Part2 example: " ++ show (part2 example)

    content <- readFile "./src/Year2024/data/day21.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
