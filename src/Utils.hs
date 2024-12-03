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

zipWithNext :: [a] -> [(a, a)]
zipWithNext xs = zip xs $ tail xs
