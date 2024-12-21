module Year2024.Day19 (solve) where

import Data.List (isPrefixOf, sortBy)
import Data.List.Split (splitOn)
import Data.MemoTrie (memo)
import Debug.Trace (trace)
import Utils (strip)

{- | Part 1

Example:

>>> let example = "r, wr, b, g, bwu, rb, gb, br\n\nbrwrr\nbggr\ngbbr\nrrbgbr\nubwu\nbwurrg\nbrgr\nbbrgwb"
>>> part1 example
6
-}
part1 :: String -> Int
part1 s = length . filter (`isPossible` fTowels) $ patterns
  where
    (towels, patterns) = pInput s
    fTowels =
        filterTowels []
            . sortBy (\a b -> compare (length a) (length b))
            $ towels

-- >>> filterTowels [] ["r","wr","b","g","bwu","rb","gb","br"]
-- ["bwu","g","b","wr","r"]
filterTowels :: [String] -> [String] -> [String]
filterTowels rts [] = rts
filterTowels rts (t : ts) =
    if isPossible (Pattern t) rts
        then filterTowels rts ts
        else filterTowels (t : rts) ts

newtype Pattern = Pattern String deriving (Show, Eq)

-- data SearchP a = SearchP a (a -> S.Set a) (a -> Bool)

-- >>> isPossible (Pattern "bbrgwb") ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"]
-- >>> isPossible (Pattern "bggr") ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"]
-- False
-- True
isPossible :: Pattern -> [String] -> Bool
isPossible _ [] = False
isPossible (Pattern "") _ = True
isPossible (Pattern p) ts =
    any (\t -> isPossible (newPattern t) ts) $
        filter (`isPrefixOf` p) ts
  where
    newPattern t = Pattern (drop (length t) p)

{- | Part 2

Example should be 16:

>>> let example = "r, wr, b, g, bwu, rb, gb, br\n\nbrwrr\nbggr\ngbbr\nrrbgbr\nubwu\nbwurrg\nbrgr\nbbrgwb"
>>> part2 example
16
-}
part2 :: String -> Int
part2 s =
    sum
        . map
            ( \p ->
                let n = countPossibilities towels p
                 in trace (show p ++ " - " ++ show n) $ n
            )
        . filter (`isPossible` fTowels)
        $ patterns
  where
    (towels, patterns) = pInput s
    fTowels =
        filterTowels []
            . sortBy (\a b -> compare (length a) (length b))
            $ towels

-- >>> let towels = ["grbb", "burb", "wrwbrwg", "uwwb", "bwbbw", "ubgrbu", "gguu", "uru", "gwr", "wrw", "gubwb", "g", "gwbu", "rbw", "bbuu", "rwgbr", "urrr", "rwww", "wrrb", "ug", "rubbwuuu", "gbrbr", "brb", "wrubb", "gwrgbbgu", "wggwbrww", "rwwb", "br", "buuwr", "rgrbb", "wgubb", "gbb", "gbrb", "rubw", "ubr", "guu", "wrugbg", "gubwru", "bww", "rbu", "burr", "ugu", "bbggguw", "bguw", "ubu", "uuuw", "uugww", "urugr", "uwgur", "gugrgggw", "b", "gugu", "wgb", "rwgwuu", "guw", "brg", "ubur", "wbwwb", "ggbg", "wr", "urw", "wur", "brwgrw", "uuur", "rwwg", "rrbrbggw", "gwrurwg", "rurru", "r", "bug", "ruwrww", "brgwu", "bwgurbu", "rwuu", "guub", "rrbwb", "rbgwbr", "grbrugbu", "bwugwugb", "guwww", "ugwggg", "buggrug", "urb", "bbbwuur", "ururgur", "wu", "bbbru", "ruuw", "grr", "buw", "bub", "gbwwug", "rwrbw", "wrbg", "wubr", "u", "wgrb", "wbuub", "ugrugwg", "wggbrr", "gr", "uur", "ugb", "wbg", "uub", "bbbrww", "uwbr", "buwguww", "wbwgw", "ubuuwr", "wuuwgr", "wggw", "gwurgub", "rwwu", "bruwg", "bgbrbwb", "rbgg", "rbg", "bubggwu", "gubgrur", "gwb", "rgu", "wrr", "uug", "bgw", "ruuugu", "uwbg", "rgurgwwb", "wrwgb", "brbbr", "wgrgu", "brgbwb", "gub", "uugw", "wrrbb", "wwruuru", "grrw", "urgg", "gbwg", "ubbur", "bggwwgw", "ubggw", "uwgw", "ruu", "wwuu", "bwuu", "uggw", "gwu", "urg", "rwbb", "uwwub", "ruug", "bwbbu", "wbb", "bwwwu", "rbwrruu", "gg", "ugrbbwgb", "wbbwwr", "bbwrrrb", "rgwwu", "bbr", "wbuwb", "grub", "ubwwgu", "ubb", "brrbuu", "grw", "bwugg", "wurbu", "uwrb", "rwgg", "rurbuu", "gbrug", "brwgr", "gru", "bgrub", "uuwr", "gbwww", "bbrw", "rbr", "bwrg", "ruur", "gbbgr", "brgwug", "ugg", "bwgubu", "rrg", "ub", "gbuwu", "ubuwuw", "ggw", "wrgww", "uuwb", "bwr", "bb", "ruwu", "bwugrw", "buu", "wug", "gggwbwu", "brurg", "rrur", "gb", "wg", "ggruwbu", "gbg", "bruur", "gug", "rgr", "gwuuu", "uwwurr", "ubw", "uugbuw", "ruugwu", "bu", "ubrbw", "ggu", "rug", "wgurwurr", "ugrrbr", "brgrb", "rrbrgwww", "rrgg", "grb", "brbw", "bruwgw", "uwbw", "rgg", "wrg", "rrruwr", "gguw", "bugbbrw", "bbg", "rwg", "wbuwg", "uuu", "wruwur", "ugwb", "bur", "urgb", "bg", "uwubrwu", "rwbwwg", "rub", "wwwwbw", "rbbugbr", "wgg", "bwbb", "brw", "ggugb", "guggw", "bgg", "wwr", "gwrbb", "rbb", "bgb", "brrubwb", "wwu", "bggrrwu", "bugb", "gwugw", "grg", "wrb", "wuub", "bru", "gggrr", "wuw", "uwg", "gbgurg", "wguwug", "gbwgw", "bgrb", "wguuw", "rwr", "ubuww", "wruuw", "wurur", "wuu", "bbw", "brbgubb", "bubg", "wgw", "wwuwrw", "bbwu", "bbrgubw", "uwu", "bw", "wbr", "bbu", "bgr", "ubg", "rwrgg", "uggu", "wugggb", "wwbgguur", "wrwuwb", "rgrr", "wgbu", "ggb", "ubuw", "rgugg", "gwuru", "guuubbw", "bgbrrw", "rrgwg", "gbw", "wbu", "wuwg", "gugb", "uww", "ruugb", "rubu", "ugwbw", "bbrgrg", "grbrr", "guwgruwr", "wwbr", "wgruw", "grwg", "wgr", "ururwr", "gbbg", "gbuub", "uwr", "grwgrwr", "ruw", "wru", "ggr", "guwuu", "uwuuwu", "gbu", "rwbbgg", "ruruww", "wwwur", "rgrug", "rbbwg", "rgwugwg", "wbw", "bbru", "ggg", "bbuugrrg", "uugb", "rw", "gburwww", "rububugg", "gguubbg", "rrrurwwg", "buruwrb", "bbwrggwb", "gbrbwr", "uruwwu", "wwwurbu", "rrwb", "bbb", "bwg", "wurggubb", "wgu", "gwwg", "wururruu", "bbbu", "ur", "rwb", "uwww", "wgwr", "rr", "rru", "uuw", "uu", "bwgw", "brr", "wub", "bwgbrrw", "gwrgrw", "uw", "ggguwbg", "bubbur", "ubbggu", "ugr", "rrrw", "ru", "rb", "gwwbgw", "rrr", "rrw", "gruw", "rwrrbb", "rugr", "uubw", "ruwb", "uubbbu", "gur", "wb", "rrrrwww", "bbub", "bgurwg", "ubbugw", "gwbw", "gggbru", "ugru", "brru", "ggwg", "wwg", "ugubr", "urggbu", "ggwuww", "gbr", "guwubu", "rwu", "gwg", "rrbur", "gwrg", "wwubr", "uwb", "gbrr", "rurburb", "brrg", "ggur", "rurbw", "uwbu", "grbbu", "wgur", "urr", "rgggg", "rrb", "ww", "rww", "bwwwr", "wubwubgw", "ggubw", "wwb", "uurb", "wbrggwu", "ugw", "rggwwr", "urwu", "gwwggg", "uwuubwr", "gwuubww", "uwbwu", "www", "gwgwr", "bwu", "gu", "rur", "wwwg", "uwruub"]
-- >>> let pattern = Pattern "rrwuwuurrrguggbrrrrgwwururguggwuwrwwurwuuwbw"
-- >>> memoCountPossibilities towels pattern
memoCountPossibilities :: [String] -> Pattern -> Int
memoCountPossibilities ts (Pattern p) = memo go p
  where
    go "" = 1
    go p' =
        sum . map (go . newPattern) $ filter (`isPrefixOf` p') ts
      where
        newPattern t = drop (length t) p'

-- >>> countPossibilities (Pattern "gbbr") ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"]
-- 4
countPossibilities :: [String] -> Pattern -> Int
countPossibilities [] _ = 0
countPossibilities _ (Pattern "") = 1
countPossibilities ts (Pattern p) =
    sum . map (countPossibilities ts . newPattern) $
        filter (`isPrefixOf` p) ts
  where
    newPattern t = Pattern (drop (length t) p)

-- >>> let example = "r, wr, b, g, bwu, rb, gb, br\n\nbrwrr\nbggr\ngbbr\nrrbgbr\nubwu\nbwurrg\nbrgr\nbbrgwb"
-- >>> pInput example
-- (["r","wr","b","g","bwu","rb","gb","br"],[Pattern "brwrr",Pattern "bggr",Pattern "gbbr",Pattern "rrbgbr",Pattern "ubwu",Pattern "bwurrg",Pattern "brgr",Pattern "bbrgwb"])
pInput :: String -> ([String], [Pattern])
pInput s = (towels, patterns)
  where
    towels = map strip . splitOn "," . head . lines $ s
    patterns = map Pattern . drop 2 . lines $ s

solve :: IO ()
solve = do
    -- example <- readFile "./src/Year2024/data/day19-example.txt"
    -- print $ "Part1 example: " ++ show (part1 example)
    -- print $ "Part2 example: " ++ show (part2 example)

    content <- readFile "./src/Year2024/data/day19.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
