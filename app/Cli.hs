{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Cli where

import Control.Applicative ((<**>))
import Options.Applicative (
    InfoMod,
    Parser,
    ParserInfo,
    auto,
    command,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    subparser,
 )

data Type = Calendar | Puzzle

data Options (t :: Type) where
    CalendarO :: {cYear :: Int} -> Options 'Calendar
    PuzzleO :: {pYear :: Int} -> Options 'Puzzle

data Command where
    CalendarC :: Options 'Calendar -> Command
    PuzzleC :: Options 'Puzzle -> Command

progParser :: ParserInfo Command
progParser =
    info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Advent of Code command line tool"
            <> header "Access the calendar puzzles and leaderboard from the command line"
        )

commandParser :: Parser Command
commandParser =
    subparser
        ( command "calendar" (CalendarC <$> calendarOptions)
            <> command "puzzle" (PuzzleC <$> puzzleOptions)
        )

calendarOptions :: ParserInfo (Options 'Calendar)
calendarOptions =
    info
        (CalendarO <$> yearParser <**> helper)
        calendarDescription

calendarDescription :: InfoMod a
calendarDescription =
    fullDesc
        <> progDesc "Show Advent of Code calendar"

puzzleOptions :: ParserInfo (Options 'Puzzle)
puzzleOptions = info (PuzzleO <$> yearParser <**> helper) puzzleDescription

puzzleDescription :: InfoMod a
puzzleDescription = fullDesc <> progDesc "Show Advent of Code puzzle"

yearParser :: Parser Int
yearParser =
    option
        auto
        ( long "year"
            <> short 'y'
            <> metavar "YEAR"
            <> help "year of the Advent of Code puzzles"
        )
