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

fetchData :: IO ()
fetchData = do
    loadFile defaultConfig
    sessionToken <- getEnv "SESSION_TOKEN"
    -- leaderBoardId <- getEnv "PRIVATE_LEADERBOARD"
    -- let url = privateLeaderBoardUrl leaderBoardId

    let url = "https://adventofcode.com/2024"
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

privateLeaderBoardUrl :: String -> String
privateLeaderBoardUrl bId = baseUrl ++ "/2024/leaderboard/private/view/" ++ bId

{-
Oh, hello!  Funny seeing you here.\n\nI appreciate your enthusiasm, but you aren't going to find much down here.\nThere certainly aren't clues to any of the puzzles.  The best surprises don't\neven appear in the source until you unlock them for real.\n\nPlease be careful with automated requests; I'm not a massive company, and I can\nonly take so much traffic.  Please be considerate so that everyone gets to play.\n\nIf you're curious about how Advent of Code works, it's running on some custom\nPerl code. Other than a few integrations (auth, analytics, social media), I\nbuilt the whole thing myself, including the design, animations, prose, and all\nof the puzzles.\n\nThe puzzles are most of the work; preparing a new calendar and a new set of\npuzzles each year takes all of my free time for 4-5 months. A lot of effort\nwent into building this thing - I hope you're enjoying playing it as much as I\nenjoyed making it for you!\n\nIf you'd like to hang out, I'm @was.tl on Bluesky, @ericwastl@hachyderm.io on\nMastodon, and @ericwastl on Twitter.\n\n- Eric Wastl\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n-->\n
<a aria-label=\"Day 1\" href=\"/2024/day/1\" class=\"calendar-day1\">       .--'     |        .-'                  '-.  <span class=\"calendar-day\"> 1</span> <span class=\"calendar-mark-complete\">*</span><span class=\"calendar-mark-verycomplete\">*</span>
<a aria-label=\"Day 2\" href=\"/2024/day/2\" class=\"calendar-day2\">    .--'        |        |                      |  <span class=\"calendar-day\"> 2</span> <span class=\"calendar-mark-complete\">*</span><span class=\"calendar-mark-verycomplete\">*</span>
<a aria-label=\"Day 3\" href=\"/2024/day/3\" class=\"calendar-day3\">.---'           |        |                      |  <span class=\"calendar-day\"> 3</span> <span class=\"calendar-mark-complete\">*</span><span class=\"calendar-mark-verycomplete\">*</span>
<a aria-label=\"Day 4\" href=\"/2024/day/4\" class=\"calendar-day4\">|               |        |                      |  <span class=\"calendar-day\"> 4</span> <span class=\"calendar-mark-complete\">*</span><span class=\"calendar-mark-verycomplete\">*</span>
<a aria-label=\"Day 5\" href=\"/2024/day/5\" class=\"calendar-day5\">|               |        |          ..          |  <span class=\"calendar-day\"> 5</span> <span class=\"calendar-mark-complete\">*</span><span class=\"calendar-mark-verycomplete\">*</span>
<a aria-label=\"Day 6\" href=\"/2024/day/6\" class=\"calendar-day6\">|               |        |        .'  '.        |  <span class=\"calendar-day\"> 6</span> <span class=\"calendar-mark-complete\">*</span><span class=\"calendar-mark-verycomplete\">*</span>
<a aria-label=\"Day 7\" href=\"/2024/day/7\" class=\"calendar-day7\">|               |        |        |    |        |  <span class=\"calendar-day\"> 7</span> <span class=\"calendar-mark-complete\">*</span><span class=\"calendar-mark-verycomplete\">*</span>
<a aria-label=\"Day 8\" href=\"/2024/day/8\" class=\"calendar-day8\">|   .--.        |        |        |    |        |  <span class=\"calendar-day\"> 8</span> <span class=\"calendar-mark-complete\">*</span><span class=\"calendar-mark-verycomplete\">*</span>
                  <span aria-hidden=\"true\" class=\"calendar-day9\">'---'  |        |        |        |    |        |  <span class=\"calendar-day\"> 9</span><span id=\"calendar-countdown\"></span><script>\n(function(){\nvar countdown = document.getElementById(\"calendar-countdown\");\nif (!countdown) return;\nvar server_eta = 40392;\nvar key = \"2024-9-\"+server_eta;\nvar now = Math.floor(new Date().getTime()/1000);\nvar target = server_eta + now;\nif (sessionStorage) {\n  // if you navigate away and hit the back button, this makes sure the countdown doesn't start from the wrong time\n  var prev_target = sessionStorage.getItem(\"calendar-target\");\n  try { prev_target = JSON.parse(prev_target); } catch(e){}\n  if (prev_target && typeof prev_target === 'object' && prev_target.key === key) {\n    target = prev_target.target;\n  } else {\n    sessionStorage.setItem(\"calendar-target\", JSON.stringify({key:key, target:target+1}));\n  }\n}\n\nvar interval = null;\nfunction update_countdown() {\n  var remaining = Math.ceil(target - new Date().getTime()/1000);\n  if (remaining <= 0) {\n    clearInterval(interval);\n    interval = null;\n    countdown.textContent = \"\";\n\n    var a = document.createElement(\"a\");\n    a[String.fromCharCode(104,114,101,102)] = \"/2024\" + String.fromCharCode(47,100,97,121,47) + \"9\";\n    a.className = \"calendar-day9 calendar-day-new\";\n    var span = countdown.parentNode;\n    while (span.firstChild) {\n      a.appendChild(span.firstChild);\n    }\n    a.appendChild(document.createTextNode(\"   \"));\n    span.parentNode.insertBefore(a, span);\n    span.parentNode.removeChild(span);\n    countdown.parentNode.removeChild(countdown);\n  } else {\n    var hours = Math.floor(remaining/60/60);\n    remaining -= hours * 60 * 60;\n    var minutes = Math.floor(remaining/60);\n    remaining -= minutes * 60;\n    var seconds = remaining;\n    countdown.textContent = (hours < 10 ? \"0\" : \"\") + hours + \":\" + (minutes < 10 ? \"0\" : \"\") + minutes + \":\" + (seconds < 10 ? \"0\" : \"\") + seconds;\n  }\n}\ninterval = setInterval(update_countdown,1000);\nupdate_countdown();\n})();\n</script></span>
                  <span aria-hidden=\"true\" class=\"calendar-day10\">                                                   <span class=\"calendar-day\">10</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day11\">                                                   <span class=\"calendar-day\">11</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day12\">                                                   <span class=\"calendar-day\">12</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day13\">                                                   <span class=\"calendar-day\">13</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day14\">                                                   <span class=\"calendar-day\">14</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day15\">                                                   <span class=\"calendar-day\">15</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day16\">                                                   <span class=\"calendar-day\">16</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day17\">                                                   <span class=\"calendar-day\">17</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day18\">                                                   <span class=\"calendar-day\">18</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day19\">                                                   <span class=\"calendar-day\">19</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day20\">                                                   <span class=\"calendar-day\">20</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day21\">                                                   <span class=\"calendar-day\">21</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day22\">                                                   <span class=\"calendar-day\">22</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day23\">                                                   <span class=\"calendar-day\">23</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day24\">                                                   <span class=\"calendar-day\">24</span></span>
                  <span aria-hidden=\"true\" class=\"calendar-day25\">                                                   <span class=\"calendar-day\">25</span></span>
-}
