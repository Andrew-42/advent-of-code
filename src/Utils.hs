module Utils where

import qualified Data.Map as M
import qualified Data.Set as S

-- STRING

wschars :: String
wschars = " \t\r\n"

{- | Remove leading whitespace
>>> lstrip "   test word   "
"test word   "
-}
lstrip :: String -> String
lstrip = dropWhile (`elem` wschars)

{- | Remove ending whitespace
>>> rstrip "   test word   "
"   test word"
-}
rstrip :: [Char] -> [Char]
rstrip = reverse . lstrip . reverse

{- | Remove leading and ending whitespace
>>> strip "   test word   "
"test word"
-}
strip :: [Char] -> String
strip = lstrip . rstrip

replace :: Char -> Char -> String -> String
replace p r = map (\c -> if p == c then r else c)

-- LIST

-- >>> 2 `itemAt` [75,47,61,53,29]
-- >>> 4 `itemAt` [75,47,61,53,29]
-- Just 61
-- Just 29
itemAt :: Int -> [a] -> Maybe a
itemAt _ [] = Nothing
itemAt n xs = if (0 <= n) && (n < length xs) then Just (xs !! n) else Nothing

-- >>> setItemAt 2 2 [75,47,61,53,29]
-- >>> setItemAt 4 2 [75,47,61,53,29]
-- Just [75,47,2,53,29]
-- Just [75,47,61,53,2]
setItemAt :: Int -> a -> [a] -> Maybe [a]
setItemAt _ _ [] = Nothing
setItemAt n v xs =
    if (0 <= n) && (n < length xs)
        then Just (take n xs ++ [v] ++ drop (n + 1) xs)
        else Nothing

{- | Count elements passing predicate in list
>>> countIf (== 'b') "abcdeabcfgh"
2
-}
countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

-- >>> dropLast 3 "abcdef"
-- "abc"
dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse

-- >>> takeLast 3 "abcdef"
-- "def"
takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

-- >>> windowed 3 sum [1, 2, 1, 1, 1]
-- [4,4,3,2,1]
windowed :: Int -> ([a] -> b) -> [a] -> [b]
windowed _ _ [] = []
windowed n f xs = f (take n xs) : windowed n f (tail xs)

{- | Create a list of pairs
>>> zipWithNext [1, 2, 3, 4]
[(1,2),(2,3),(3,4)]
-}
zipWithNext :: [a] -> [(a, a)]
zipWithNext xs = zip xs $ tail xs

-- Matrix

{- | Matrix transpose
We expect all the lists to have the same length

>>> transpose ["abc", "abc", "abc"]
["aaa","bbb","ccc"]
-}
transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose xs = map head xs : transpose (map tail xs)

-- SET

toSet :: (Ord a) => [a] -> S.Set a
toSet = S.fromList

-- MAP

-- >>> freq "aabcdddeeefggh"
-- fromList [('a',2),('b',1),('c',1),('d',3),('e',3),('f',1),('g',2),('h',1)]
freq :: (Ord a) => [a] -> M.Map a Int
freq = foldr (\l m -> M.insertWith (+) l 1 m) M.empty
