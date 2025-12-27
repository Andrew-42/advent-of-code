{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import qualified Data.Heap as H
import qualified Data.Map as M
import qualified Data.Set as S

{-
 - a - the solution
 - b - the statespace
 - step function
 - is finished function
 -}
data SearchProblem a b = SearchProblem a b (a -> b -> ([a], b)) (a -> Bool)

bfs :: (Ord a) => SearchProblem a b -> a
bfs (SearchProblem start g step finished) = loop (S.singleton start) g
  where
    loop xs g'
        | finished h = h
        | otherwise = loop (S.union rest (S.fromList nr)) ng
      where
        (nr, ng) = step h g'
        (h, rest) = pop xs

safeBfs :: (Ord a) => SearchProblem a b -> Maybe a
safeBfs (SearchProblem start g step finished) = loop (S.singleton start) g
  where
    loop xs g'
        | S.null xs = Nothing
        | finished h = Just h
        | otherwise = loop (S.union rest (S.fromList nr)) ng
      where
        (nr, ng) = step h g'
        (h, rest) = pop xs

pop :: S.Set a -> (a, S.Set a)
pop xs = (S.elemAt 0 xs, S.deleteAt 0 xs)

class (Ord k) => Node a k | a -> k where
    priority :: a -> Int
    distance :: a -> Int
    key :: a -> k

dijkstra :: (Node a k) => SearchProblem a b -> a
dijkstra (SearchProblem start g step finished) = loop initCache initHeap g
  where
    initHeap = H.singleton $ H.Entry (priority start) start
    initCache = M.singleton (key start) 0

    loop cache xs g'
        | finished h = h
        | otherwise = loop newCache newHeap ng
      where
        (nr, ng) = step h g'
        (H.Entry _ h, rest) = case H.uncons xs of
            Just (h', rest') -> (h', rest')
            Nothing -> error "Should never happen"
        nr' = filter (isGood cache) nr
        newCache = M.union (M.fromList $ map (\r -> (key r, distance r)) nr') cache
        newHeap = rest <> H.fromList (map (\r -> H.Entry (priority r) r) nr')

    isGood cache' n = case M.lookup (key n) cache' of
        Just d -> distance n < d
        Nothing -> True
