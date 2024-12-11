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

data Command = Calendar CalendarOptions | Puzzle PuzzleOptions

data CalendarOptions = CalendarOptions {calendarYear :: Int}
data PuzzleOptions = PuzzleOptions {puzzleYear :: Int}

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
        ( command "calendar" (Calendar <$> calendarOptions)
            <> command "puzzle" (Puzzle <$> puzzleOptions)
        )

calendarOptions :: ParserInfo CalendarOptions
calendarOptions =
    info
        (CalendarOptions <$> yearParser <**> helper)
        calendarDescription

calendarDescription :: InfoMod a
calendarDescription =
    fullDesc
        <> progDesc "Show Advent of Code calendar"

puzzleOptions :: ParserInfo PuzzleOptions
puzzleOptions = info (PuzzleOptions <$> yearParser <**> helper) puzzleDescription

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
