module Search where

import qualified Data.Set as S

{-
 - a - the solution
 - b - the statespace
 - step function
 - is finished function
 -}
data SearchProblem a b = SearchProblem a b (a -> b -> (S.Set a, b)) (a -> Bool)

bfs :: (Ord a) => SearchProblem a b -> a
bfs (SearchProblem start g step finished) = loop (S.singleton start) g
  where
    loop xs g'
        | finished h = h
        | otherwise = loop (S.union rest nr) ng
      where
        (nr, ng) = step h g'
        (h, rest) = pop xs

safeBfs :: (Ord a) => SearchProblem a b -> Maybe a
safeBfs (SearchProblem start g step finished) = loop (S.singleton start) g
  where
    loop xs g'
        | S.null xs = Nothing
        | finished h = Just h
        | otherwise = loop (S.union rest nr) ng
      where
        (nr, ng) = step h g'
        (h, rest) = pop xs

pop :: S.Set a -> (a, S.Set a)
pop xs = (S.elemAt 0 xs, S.deleteAt 0 xs)

-- dijkstra :: (Ord a) => SearchProblem a b -> a
-- dijkstra (SearchProblem start g step finished) = loop (Q.singleton start) g
--   where
--     loop xs g'
--         | finished h = h
--         | otherwise = loop (Q.union rest nr) ng
--       where
--         (nr, ng) = step h g'
--         (h, rest) = pop xs
