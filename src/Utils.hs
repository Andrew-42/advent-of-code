module Utils where

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

{- | Matrix transpose
We expect all the lists to have the same length

>>> transpose ["abc", "abc", "abc"]
["aaa","bbb","ccc"]
-}
transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose xs = map head xs : transpose (map tail xs)

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
