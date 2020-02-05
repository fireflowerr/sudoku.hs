{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sudoku.Sudoku where
import Control.Monad (guard)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Function (fix)
import Data.STRef
import Sudoku.Internal
import System.Random
import qualified  Data.Set as Set

type Board = UArray (Int, Int) Int -- +-+-+-+
                                  -- |1|2|3|
                                  -- +-----+
                                  -- |4|5|6|
                                  -- +-----+
                                  -- |7|8|9|
                                  -- +-+-+-+

type ScanArea = ([Int], [Int], [Int]) -- Rows to scan, colums to scan, blocks

type Possibility = ((Int, Int), Int)

sudokuSz :: Int
sudokuSz = 9

blockWidth :: Int
blockWidth = 3

scanAll :: ScanArea
scanAll = let !r = [1..sudokuSz] in (r, r, r)

-- | Return the index of the coordinate's block
getBlock :: (Int, Int) -> Int
getBlock (i, j) = i' + j' where
    i' = (i - 1) `div` blockWidth * blockWidth + 1
    j' = (j - 1) `div` blockWidth

-- | Return all indicies of a given block
blockIndices :: Int -> [(Int, Int)]
blockIndices n = is `distrubute` js where
    distrubute is' js' = [ (i, j) | i <- is', j <- js' ]
    is = let del = (n - 1) `div` blockWidth  * blockWidth
        in [del + 1 .. blockWidth + del]
    js = let del = (n - 1) `mod` blockWidth  * blockWidth
        in [del + 1 .. blockWidth + del]

-- | Return all indicies of a given row
rowIndices :: Int -> [(Int, Int)]
rowIndices i = [(i, j) | j <- [1 .. sudokuSz]]

-- | Return all indicies of a given column
colIndices :: Int -> [(Int, Int)]
colIndices j = [(i, j) | i <- [1 .. sudokuSz]]

-- | Return true if a list of integers >= 0 contains no duplicates O(n)
canidate :: Int -> [Int] -> Bool -> Bool
canidate mx xs z = runST do
    arr <- newArray (1, mx) False :: ST s (STUArray s Int Bool)
    i <- newSTRef xs
    fix $ \go -> do
        r <- readSTRef i
        case r of
            []     -> return True
            (0:us) -> do -- 0 = unfilled
                if z
                    then return False
                    else writeSTRef i us >> go
            (u:us) -> do
                tmp <- readArray arr u
                writeSTRef i us
                writeArray arr u True
                if tmp then return False else go

-- | Return true if the area scanned over could potentially form part of a valid
-- sudoku solution
validateArea :: ScanArea -> Board -> Bool -> Bool
validateArea (row, col, blk) g z = and $ map f r <> map f c <> map f b where
    validate k xs = map (g !) . k <$> xs
    f = flip (canidate sudokuSz) z
    r = validate rowIndices row
    c = validate colIndices col
    b = validate blockIndices blk

-- | Find all indices in the grid with a given value
findIndices :: Int -> Board -> [(Int, Int)]
findIndices n arr = map fst . filter (\x -> snd x == n) . assocs $ arr

-- | Produces the list of all values over a list of indices given a generator
-- and seed.
fetch :: Board -> (Int -> [(Int, Int)]) -> Int -> [Int]
fetch g f n = (g !) <$> f n

type Prune = (Possibility -> Bool)

-- | Creates a function that returns true if a point is in the same row, col, or
-- block as the provided point
pruneDepends :: Possibility -> Prune
pruneDepends (c, v) = \(c', v') -> if v == v'
    then fst c == fst c'
        || snd c == snd c'
        || getBlock c == getBlock c'
    else False

-- | Composition of pruning predicates under OR
composePrune :: Prune -> Prune -> Prune
composePrune a b = \x -> b x || a x

-- discards branches of invalid posibilties using prunes. (like a dfs over a
-- multitree)
-- | Generate all possible solutions
forest :: [((Int, Int), [Int])] -> [[Possibility]]
forest [] = []
forest ((c', vs') : xs') = map (map fst)
    $ f xs' [[((c', v'), pruneDepends (c', v'))] | v' <- vs'] where
        f [] acc = acc
        f ((c, vs) : xs) acc = f xs [ g (c, v) u x
            | (x@((_,u):_)) <- acc, v <- vs]
        g p u xs = if u p
            then []
            else (p, composePrune (pruneDepends p) u) : xs

-- | Returns (row, column, and block) index of a given point
pointUnit :: (Int, Int) -> (Int, Int, Int)
pointUnit x = (fst x, snd x, getBlock x)

-- | Takes a list unit indexes (pointUnit) and returns the
-- values of those sectors
unitVals :: Board -> (Int, Int, Int) -> ([Int], [Int], [Int])
unitVals g (a, b, c) = ( fetch g rowIndices a
                       , fetch g colIndices b
                       , fetch g blockIndices c )

-- | Returns all mising values in a sudoku unit vector
missingVals :: ([Int], [Int], [Int]) -> [Int]
missingVals (a, b, c) = Set.toList $ Set.difference full some where
    full = Set.fromList [1 .. sudokuSz]
    some = Set.unions $ Set.fromList <$> [a, b, c]

-- | Returns all possible solutions
solve :: Board -> StdGen -> Maybe [[Possibility]]
solve g rand
    | validateArea scanAll g False = let
        is = findIndices 0 g
        -- Shuffling the seed dec time on avg
        f a b = (a, fst $ shuffle' (missingVals b) rand)
        -- Possible solutions at each index
        rs = zipWith f is $ unitVals g . pointUnit <$> is
        result = if null is
            then (True, [])
            else (False,)  $ do
                sltn <- forest rs :: [[Possibility]]
                let test' = g  // sltn
                guard $ validateArea scanAll test' True
                return sltn
        in case result of -- handle no result found case
            (True, x) -> Just x
            (_,  [])  -> Nothing
            (_,  xs)  -> Just xs
    | otherwise = Nothing
