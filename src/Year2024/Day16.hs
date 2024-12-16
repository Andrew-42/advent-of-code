module Year2024.Day16 (solve) where

import qualified Data.Set as S
import Grid (
    Direction (East),
    Grid2D (Grid2D),
    Position,
    gridAt,
    next,
    positionOf,
    setGridAt,
    turnLeft,
    turnRight,
 )

{- | Part 1

Example should be 7036:

>>> let example = "###############\n#.......#....E#\n#.#.###.#.###.#\n#.....#.#...#.#\n#.###.#####.#.#\n#.#.#.......#.#\n#.#.#####.###.#\n#...........#.#\n###.#.#####.#.#\n#...#.....#.#.#\n#.#.#.###.#.#.#\n#.....#...#.#.#\n#.###.#.#.#.#.#\n#S..#.....#...#\n###############"
>>> part1 example
Just 7036
-}
part1 :: String -> Maybe Int
part1 s = do
    (r, e, g) <- pInput s
    let sp = SearchProblem r g advance (isDone e)
    let (Reindeer score _ _ _) = bfs sp
    return score

data Reindeer = Reindeer Int Position Direction [Position]
    deriving (Show, Eq, Ord)

data SearchProblem a b = SearchProblem a b (a -> b -> (S.Set a, b)) (a -> Bool)

bfs :: SearchProblem Reindeer (Grid2D Char) -> Reindeer
bfs (SearchProblem start g step finished) = loop (S.singleton start) g
  where
    loop :: S.Set Reindeer -> Grid2D Char -> Reindeer
    loop xs g'
        | finished h = h
        | otherwise = loop (S.union rest nr) ng
      where
        (nr, ng) = step h g'
        (h, rest) = pop xs

pop :: S.Set a -> (a, S.Set a)
pop xs = (S.elemAt 0 xs, S.deleteAt 0 xs)

advance :: Reindeer -> Grid2D Char -> (S.Set Reindeer, Grid2D Char)
advance r@(Reindeer s rp d ps) g = case gridAt np g of
    Just '#' -> (S.fromList rRs, force (setGridAt 'O' rp g))
    Just '.' ->
        (S.fromList $ Reindeer (s + 1) np d (np : ps) : rRs, force (setGridAt 'O' np g))
    Just _ -> (S.empty, g)
    Nothing -> (S.empty, g)
  where
    np = next rp d
    rRs = rotate r g turnLeft ++ rotate r g turnRight

force :: Maybe a -> a
force (Just x) = x
force Nothing = error "The value must be just."

rotate :: Reindeer -> Grid2D Char -> (Direction -> Direction) -> [Reindeer]
rotate (Reindeer s rp d ps) g rt = case gridAt np g of
    Just '.' -> [Reindeer (s + 1000) rp (rt d) ps]
    Just _ -> []
    Nothing -> []
  where
    np = next rp (rt d)

isDone :: Position -> Reindeer -> Bool
isDone p (Reindeer _ rp _ _) = p == rp

{- | Part 2

Example:

>>> let example = "###############\n#.......#....E#\n#.#.###.#.###.#\n#.....#.#...#.#\n#.###.#####.#.#\n#.#.#.......#.#\n#.#.#####.###.#\n#...........#.#\n###.#.#####.#.#\n#...#.....#.#.#\n#.#.#.###.#.#.#\n#.....#...#.#.#\n#.###.#.#.#.#.#\n#S..#.....#...#\n###############"
>>> part2 example 7036
Just 37
-}
part2 :: String -> Int -> Maybe Int
part2 s score = do
    (r, e, g) <- pInput s
    let sp = SearchProblemM r (advance' g) (isDone e) (validScore score)
    let rs = bfsMax sp
    return $ S.size . S.fromList . concatMap takePositions $ rs

data SearchProblemM a
    = SearchProblemM a (a -> [a]) (a -> Bool) (a -> Bool)

bfsMax :: SearchProblemM Reindeer -> [Reindeer]
bfsMax (SearchProblemM start step finished endF) = loop [] [start]
  where
    loop :: [Reindeer] -> [Reindeer] -> [Reindeer]
    loop res [] = res
    loop res (x : xs)
        | finished x = loop (x : res) xs
        | otherwise = loop res (xs ++ filter endF (step x))

validScore :: Int -> Reindeer -> Bool
validScore m (Reindeer s _ _ _) = s <= m

advance' :: Grid2D Char -> Reindeer -> [Reindeer]
advance' g r@(Reindeer s rp d ps) = case gridAt np g of
    Just '#' -> rRs
    Just '.' -> Reindeer (s + 1) np d (np : ps) : rRs
    Just _ -> []
    Nothing -> []
  where
    np = next rp d
    rRs = rotate r g turnLeft ++ rotate r g turnRight

takePositions :: Reindeer -> [Position]
takePositions (Reindeer _ _ _ ps) = ps

pInput :: String -> Maybe (Reindeer, Position, Grid2D Char)
pInput s = do
    let g = Grid2D . lines $ s
    rp <- positionOf 'S' g
    g1 <- setGridAt '.' rp g
    ep <- positionOf 'E' g1
    g2 <- setGridAt '.' ep g1
    return (Reindeer 0 rp East [rp], ep, g2)

solve :: IO ()
solve = do
    example <- readFile "./src/Year2024/data/day16-test.txt"
    print $ "Part1 example: " ++ show (part1 example)
    print $ "Part2 example: " ++ show (part2 example 11048)

    content <- readFile "./src/Year2024/data/day16.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content 90460)
