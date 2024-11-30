module Utils where

wschars :: String
wschars = " \t\r\n"

lstrip :: String -> String
lstrip = dropWhile (`elem` wschars)

rstrip :: [Char] -> [Char]
rstrip = reverse . lstrip . reverse

strip :: [Char] -> String
strip = lstrip . rstrip
