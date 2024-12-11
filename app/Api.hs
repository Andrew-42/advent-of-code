{-# LANGUAGE OverloadedStrings #-}

module Api where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Lens
import qualified Control.Lens as B
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wreq
import System.Environment (getEnv)
import Text.HTML.TagSoup

baseUrl :: String
baseUrl = "https://adventofcode.com"

-- main = do
--     (Calendar (CalendarOptions year)) <- execParser progParser
--     fetchData year

fetchData :: Int -> IO ()
fetchData year = do
    loadFile defaultConfig
    sessionToken <- getEnv "SESSION_TOKEN"
    -- leaderBoardId <- getEnv "PRIVATE_LEADERBOARD"
    -- let url = privateLeaderBoardUrl leaderBoardId

    let url = "https://adventofcode.com/" ++ show year
    let opts =
            defaults
                & header "Cookie" .~ [TE.encodeUtf8 . T.pack $ "session=" ++ sessionToken]
    response <- getWith opts url
    let status = response ^. responseStatus
    let body = T.unpack $ TE.decodeUtf8 $ BS.toStrict $ response ^. responseBody

    print status

    let calendarTag = ("<pre class=calendar>" :: String)
    let closeCalendarTag = ("</pre>" :: String)
    let script = ("<script>" :: String)
    let closeScript = ("</script>" :: String)
    let calendar =
            takeWhile (~/= closeCalendarTag)
                . dropWhile (~/= calendarTag)
                . parseTags
                $ body
    let tgs = takeWhile (~/= script) calendar ++ dropWhile (~/= closeScript) calendar

    mapM_ putStrLn $ lines $ innerText tgs

privateLeaderBoardUrl :: Int -> String -> String
privateLeaderBoardUrl year bId = baseUrl ++ "/" ++ show year ++ "/leaderboard/private/view/" ++ bId
