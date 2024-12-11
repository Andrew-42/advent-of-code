module Year2024.Day09 (solve) where

import Data.Char (digitToInt)
import Utils (dropLast)

{- | Part 1

Another push of the button leaves you in the familiar hallways of some friendly
amphipods! Good thing you each somehow got your own personal mini submarine.
The Historians jet away in search of the Chief, mostly by driving directly into
walls.

While The Historians quickly figure out how to pilot these things, you notice an
amphipod in the corner struggling with his computer. He's trying to make more
contiguous free space by compacting all of the files, but his program isn't
working; you offer to help.

He shows you the disk map (your puzzle input) he's already generated. For
example:

2333133121414131402

The disk map uses a dense format to represent the layout of files and free space
on the disk. The digits alternate between indicating the length of a file and
the length of free space.

So, a disk map like 12345 would represent a one-block file, two blocks of free
space, a three-block file, four blocks of free space, and then a five-block
file. A disk map like 90909 would represent three nine-block files in a row
(with no free space between them).

Each file on disk also has an ID number based on the order of the files as they
appear before they are rearranged, starting with ID 0. So, the disk map 12345
has three files: a one-block file with ID 0, a three-block file with ID 1, and
a five-block file with ID 2. Using one character for each block where digits are
the file ID and . is free space, the disk map 12345 represents these individual
blocks:

0..111....22222

The first example above, 2333133121414131402, represents these individual
blocks:

00...111...2...333.44.5555.6666.777.888899

The amphipod would like to move file blocks one at a time from the end of the
disk to the leftmost free space block (until there are no gaps remaining between
file blocks). For the disk map 12345, the process looks like this:

0..111....22222
02.111....2222.
022111....222..
0221112...22...
02211122..2....
022111222......

The first example requires a few more steps:

00...111...2...333.44.5555.6666.777.888899
009..111...2...333.44.5555.6666.777.88889.
0099.111...2...333.44.5555.6666.777.8888..
00998111...2...333.44.5555.6666.777.888...
009981118..2...333.44.5555.6666.777.88....
0099811188.2...333.44.5555.6666.777.8.....
009981118882...333.44.5555.6666.777.......
0099811188827..333.44.5555.6666.77........
00998111888277.333.44.5555.6666.7.........
009981118882777333.44.5555.6666...........
009981118882777333644.5555.666............
00998111888277733364465555.66.............
0099811188827773336446555566..............

The final step of this file-compacting process is to update the filesystem
checksum. To calculate the checksum, add up the result of multiplying each of
these blocks' position with the file ID number it contains. The leftmost block
is in position 0. If a block contains free space, skip it instead.

Continuing the first example, the first few blocks' position multiplied by its
file ID number are 0 * 0 = 0, 1 * 0 = 0, 2 * 9 = 18, 3 * 9 = 27, 4 * 8 = 32, and
so on. In this example, the checksum is the sum of these, 1928.

Compact the amphipod's hard drive using the process he requested. What is the
resulting filesystem checksum?

Example should be 1928:

>>> let example = "2333133121414131402"
>>> part1 example
1928
-}
part1 :: String -> Int
part1 = checkSum . compress . toMemoryBlock . head . lines

checkSum :: [String] -> Int
checkSum xs = sum (zipWith (\c p -> p * read c) xs [0 ..])

compress :: [String] -> [String]
compress [] = []
compress (x : xs) =
    if x == "."
        then if null rest then [] else last rest : compress (dropLast 1 rest)
        else x : compress xs
  where
    rest = reverse . dropWhile (== ".") . reverse $ xs

toMemoryBlock :: String -> [String]
toMemoryBlock s = toBlock s 0
  where
    toBlock :: String -> Int -> [String]
    toBlock [] _ = []
    toBlock [x] fileId = fileBlock (digitToInt x) fileId
    toBlock (x : y : rest) fileId =
        fileBlock (digitToInt x) fileId
            ++ spaceBlock (digitToInt y)
            ++ toBlock rest (fileId + 1)

fileBlock :: Int -> Int -> [String]
fileBlock len = replicate len . show

spaceBlock :: Int -> [String]
spaceBlock 0 = []
spaceBlock len = replicate len "."

{- | Part 2

Upon completion, two things immediately become clear. First, the disk definitely
has a lot more contiguous free space, just like the amphipod hoped. Second, the
computer is running much more slowly! Maybe introducing all of that file system
fragmentation was a bad idea?

The eager amphipod already has a new plan: rather than move individual blocks,
he'd like to try compacting the files on his disk by moving whole files instead.

This time, attempt to move whole files to the leftmost span of free space blocks
that could fit the file. Attempt to move each file exactly once in order of
decreasing file ID number starting with the file with the highest file ID
number. If there is no span of free space to the left of a file that is large
enough to fit the file, the file does not move.

The first example from above now proceeds differently:

00...111...2...333.44.5555.6666.777.888899
0099.111...2...333.44.5555.6666.777.8888..
0099.1117772...333.44.5555.6666.....8888..
0099.111777244.333....5555.6666.....8888..
00992111777.44.333....5555.6666.....8888..

The process of updating the filesystem checksum is the same; now, this example's
checksum would be 2858.

Start over, now compacting the amphipod's hard drive using this new method
instead. What is the resulting filesystem checksum?

Example should be 2858:

>>> let example = "2333133121414131402"
>>> part2 example
2858
-}
part2 :: String -> Int
part2 = sum . map blockChecksum . blockCompress . toBlocks . head . lines

data Block
    = FileBlock {getFileId :: Int, firstCellId :: Int, getBlockSize :: Int}
    | FreeBlock {firstCellId :: Int, getBlockSize :: Int}
    deriving (Show, Eq)

blockCompress :: [Block] -> [Block]
blockCompress [] = []
blockCompress list =
    foldr defragment list (filterFiles list)
  where
    defragment :: Block -> [Block] -> [Block]
    defragment _ [] = []
    defragment f (b : bs) =
        if isMovable f b
            then moveFile f b ++ removeItem f bs
            else b : defragment f bs

filterFiles :: [Block] -> [Block]
filterFiles [] = []
filterFiles (b : bs) = case b of
    FreeBlock{} -> filterFiles bs
    f@FileBlock{} -> f : filterFiles bs

removeItem :: (Eq a) => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y : ys) = if x == y then ys else y : removeItem x ys

isMovable :: Block -> Block -> Bool
isMovable (FileBlock{}) (FileBlock{}) = False
isMovable (FreeBlock{}) _ = False
isMovable (FileBlock _ cellId blockSize) (FreeBlock freeCellId freeBlockSize) =
    blockSize <= freeBlockSize && freeCellId < cellId

moveFile :: Block -> Block -> [Block]
moveFile (FreeBlock _ _) _ = []
moveFile (FileBlock{}) (FileBlock{}) = []
moveFile (FileBlock fileId cellId blockSize) (FreeBlock freeCellId freeBlockSize)
    | cellId < freeCellId = []
    | blockSize > freeBlockSize = []
    | blockSize == freeBlockSize = [FileBlock fileId freeCellId freeBlockSize]
    | blockSize < freeBlockSize =
        [ FileBlock fileId freeCellId blockSize
        , FreeBlock (freeCellId + blockSize) (freeBlockSize - blockSize)
        ]
    | otherwise = error "The guard must be exhaustive."

blockChecksum :: Block -> Int
blockChecksum (FreeBlock _ _) = 0
blockChecksum (FileBlock fileId cellId blockSize) =
    sum . map (* fileId) $ [x + cellId | x <- [0 .. (blockSize - 1)]]

toBlocks :: String -> [Block]
toBlocks s = filter (\b -> 0 /= getBlockSize b) $ toBlock s 0 0
  where
    toBlock :: String -> Int -> Int -> [Block]
    toBlock [] _ _ = []
    toBlock [x] fileId cellId = [FileBlock fileId cellId (digitToInt x)]
    toBlock (x : y : rest) fileId cellId =
        FileBlock fileId cellId (digitToInt x)
            : FreeBlock (cellId + digitToInt x) (digitToInt y)
            : toBlock rest (fileId + 1) (digitToInt x + digitToInt y + cellId)

solve :: IO ()
solve = do
    content <- readFile "./src/Year2024/data/day09.txt"
    print $ "Part1 solution: " ++ show (part1 content)
    print $ "Part2 solution: " ++ show (part2 content)
