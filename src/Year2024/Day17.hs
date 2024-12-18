{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Year2024.Day17 (solve) where

import Data.Bits (Bits (xor))
import Data.Char (digitToInt)
import Data.List.Split (chunk)
import Debug.Trace (trace)
import Utils (itemAt)

{- | Part 1

Example:

>>> let exampleF = "Register A: 729\nRegister B: 0\nRegister C: 0\n\nProgram: 0,1,5,4,3,0"
>>> let example1 = "Register A: 0\nRegister B: 0\nRegister C: 9\n\nProgram: 2,6"
>>> let example2 = "Register A: 10\nRegister B: 0\nRegister C: 0\n\nProgram: 5,0,5,1,5,4"
>>> part1 example1
>>> part1 example2
>>> part1 exampleF
Just []
Just [0,1,2]
Just [4,6,3,5,6,3,5,2,1,0]
-}
part1 :: String -> Maybe [Int]
part1 s = do
    prg <- pInput s
    return $ getOuput $ runProgram prg

data Program
    = Program
        (Register 'A, Register 'B, Register 'C)
        InstructionPointer
        [InstructionCombo]
        Output
    deriving (Show)

newtype Output = Output [Int] deriving (Show, Eq)

newtype InstructionPointer = InstructionPointer Int deriving (Show, Eq, Ord)

data InstructionCombo = InstructionCombo OpCode Int deriving (Show, Eq)

data OpCode = Code0 | Code1 | Code2 | Code3 | Code4 | Code5 | Code6 | Code7
    deriving (Show, Eq, Ord)

data Register (t :: RegisterType) where
    RegA :: {getA :: Int} -> Register 'A
    RegB :: {getB :: Int} -> Register 'B
    RegC :: {getC :: Int} -> Register 'C

instance Show (Register t) where
    show (RegA x) = "A: " ++ show x
    show (RegB x) = "B: " ++ show x
    show (RegC x) = "C: " ++ show x

data RegisterType = A | B | C deriving (Show, Eq, Ord)

runProgram :: Program -> Program
runProgram prg = maybe prg runProgram (programStep prg)

programStep :: Program -> Maybe Program
programStep prg@(Program _ (InstructionPointer p) ics _) = do
    ic <- itemAt p ics
    return $ runInstruction ic prg

runInstruction :: InstructionCombo -> Program -> Program
runInstruction
    (InstructionCombo oc co)
    (Program (rA, rB, rC) (InstructionPointer p) ics (Output out')) = case oc of
        Code0 -> Program (adv combo rA, rB, rC) nip ics nout
        Code1 -> Program (rA, bxl combo rB, rC) nip ics nout
        Code2 -> Program (rA, bst combo, rC) nip ics nout
        Code3 -> case jnz combo rA of
            Just ip -> Program (rA, rB, rC) ip ics nout
            Nothing -> Program (rA, rB, rC) nip ics nout
        Code4 -> Program (rA, bxc rB rC, rC) nip ics nout
        Code5 -> Program (rA, rB, rC) nip ics (Output $ out' ++ [out combo])
        Code6 -> Program (rA, bdv combo rA, rC) nip ics nout
        Code7 -> Program (rA, rB, cdv combo rA) nip ics nout
      where
        nip = InstructionPointer (p + 1)
        nout = Output out'
        combo = case co of
            0 -> 0
            1 -> 1
            2 -> 2
            3 -> 3
            4 -> getA rA
            5 -> getB rB
            6 -> getC rC
            _ -> error "Invalid program."

adv :: Int -> Register 'A -> Register 'A
adv co (RegA v) = RegA (v `div` (2 ^ co))

bxl :: Int -> Register 'B -> Register 'B
bxl co (RegB v) = RegB (xor v co)

bst :: Int -> Register 'B
bst co = RegB (co `mod` 8)

jnz :: Int -> Register 'A -> Maybe InstructionPointer
jnz co (RegA v) = if v == 0 then Nothing else Just (InstructionPointer co)

bxc :: Register 'B -> Register 'C -> Register 'B
bxc (RegB vb) (RegC vc) = RegB (xor vb vc)

out :: Int -> Int
out co = co `mod` 8

bdv :: Int -> Register 'A -> Register 'B
bdv co (RegA v) = RegB (v `div` (2 ^ co))

cdv :: Int -> Register 'A -> Register 'C
cdv co (RegA v) = RegC (v `div` (2 ^ co))

{- | Part 2

Example should be 117440:

>>> let example = "Register A: 2024\nRegister B: 0\nRegister C: 0\n\nProgram: 0,3,5,4,3,0"
>>> part2 example [0,3,5,4,3,0]
Just (A: 117440,Program (A: 0,B: 0,C: 0) (InstructionPointer 2) [InstructionCombo Code0 3,InstructionCombo Code5 4,InstructionCombo Code3 0] (Output [0,3,5,4,3,0]))
-}
part2 :: String -> [Int] -> Maybe (Register 'A, Program)
part2 s sol = do
    prg <- pInput s
    let (regA, solPrg) = searchRegA 48744869 sol prg
    return (regA, solPrg)

searchRegA :: Int -> [Int] -> Program -> (Register 'A, Program)
searchRegA rA solution prg = case runProgram' prg of
    Just sol -> (getRegA prg, sol)
    Nothing -> searchRegA (rA + 1) solution (regAProgramCopy (rA + 1) prg)
  where
    runProgram' prg' = case programStep prg' of
        Just nprg@(Program _ _ _ (Output output)) ->
            if output == solution
                then Just nprg
                else
                    if continue output solution
                        then trace (show rA ++ " - " ++ show output) $ runProgram' nprg
                        else Nothing
        Nothing -> Nothing

continue :: [Int] -> [Int] -> Bool
continue o1 o2 = o1 == take (length o1) o2

getRegA :: Program -> Register 'A
getRegA (Program (regA, _, _) _ _ _) = regA

getOuput :: Program -> [Int]
getOuput (Program _ _ _ (Output output)) = output

regAProgramCopy :: Int -> Program -> Program
regAProgramCopy ra (Program (_, rB, rC) ip ics out') =
    Program (RegA ra, rB, rC) ip ics out'

pInput :: String -> Maybe Program
pInput s = do
    [ral, rbl, rcl, "", insts] <- Just $ lines s
    ["Register", "A:", nA] <- Just $ words ral
    ["Register", "B:", nB] <- Just $ words rbl
    ["Register", "C:", nC] <- Just $ words rcl
    ["Program:", nums] <- Just $ words insts
    let ops = map (\l -> (head l, last l)) . chunk 2 . filter (/= ',') $ nums
    let regA = RegA $ read nA
    let regB = RegB $ read nB
    let regC = RegC $ read nC
    let ip = InstructionPointer 0
    let out' = Output []
    let ics = map parseIC ops
    return $ Program (regA, regB, regC) ip ics out'

parseIC :: (Char, Char) -> InstructionCombo
parseIC (oc, co) = case oc of
    '0' -> InstructionCombo Code0 (digitToInt co)
    '1' -> InstructionCombo Code1 (digitToInt co)
    '2' -> InstructionCombo Code2 (digitToInt co)
    '3' -> InstructionCombo Code3 (digitToInt co)
    '4' -> InstructionCombo Code4 (digitToInt co)
    '5' -> InstructionCombo Code5 (digitToInt co)
    '6' -> InstructionCombo Code6 (digitToInt co)
    '7' -> InstructionCombo Code7 (digitToInt co)
    _ -> error "Malformed instructions"

solve :: IO ()
solve = do
    content <- readFile "./src/Year2024/data/day17.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    let solution = [2, 4, 1, 2, 7, 5, 1, 3, 4, 4, 5, 5, 0, 3, 3, 0]
    print $ "Part2 solution: " ++ show (part2 content solution)
