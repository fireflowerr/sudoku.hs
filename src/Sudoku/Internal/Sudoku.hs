{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Sudoku.Internal.Sudoku where
import Control.Monad (guard, join)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Function (fix)
import Data.Maybe (mapMaybe)
import Data.STRef
import Sudoku.Internal.Internal
import System.Random
import qualified Data.Set as Set

newtype Board = Board (UArray Point Int)      -- +-+-+-+
                                              -- |1|2|3|
type Point = (Int, Int)                       -- +-----+
                                              -- |4|5|6|
type Cell = (Point, Int)                      -- +-----+
                                              -- |7|8|9|
sudokuSz :: Int                               -- +-+-+-+
sudokuSz = 9

blockWidth :: Int
blockWidth = 3

type ScanArea = ([Int], [Int], [Int]) -- Rows to scan, columns to scan, blocks

scanAll :: ScanArea
scanAll = let !r = [1..sudokuSz] in (r, r, r)

-- | Return the index of the coordinates block
getBlock :: Point -> Int
getBlock (i, j) = i' + j' where
    i' = (i - 1) `div` blockWidth * blockWidth + 1
    j' = (j - 1) `div` blockWidth

-- | Return all indices of a given block
blockIndices :: Int -> [Point]
blockIndices n = is `distrubute` js where
    distrubute is' js' = [ (i, j) | i <- is', j <- js' ]
    is = let del = (n - 1) `div` blockWidth  * blockWidth
        in [del + 1 .. blockWidth + del]
    js = let del = (n - 1) `mod` blockWidth  * blockWidth
        in [del + 1 .. blockWidth + del]

-- | Return all indices of a given row
rowIndices :: Int -> [Point]
rowIndices i = [(i, j) | j <- [1 .. sudokuSz]]

-- | Return all indices of a given column
colIndices :: Int -> [Point]
colIndices j = [(i, j) | i <- [1 .. sudokuSz]]

-- | Return list of conflicts at each value
canidate :: [Int] -> Bool -> [(Int, [Int])]
canidate xs z = filter (\x -> (length . take 2 . snd $ x) > 1) . assocs
    $ runSTArray do
        arr <- newArray (0, sudokuSz) [] :: ST s (STArray s Int [Int])
        i <- newSTRef xs
        j <- newSTRef 1
        let appIdx d ix = do
                tmp <- readArray arr d
                writeArray arr d $ ix:tmp
        let inc to = do
                modifySTRef j (+ 1)
                modifySTRef i tail
                to
        fix $ \go -> do
            i' <- readSTRef i
            j' <- readSTRef j
            case i' of
                [] -> return ()
                (0:_) -> if z then appIdx 0 j' >> inc go else inc go
                (u:_) -> appIdx u j' >> inc go
        return arr

-- | Given block index & offset return the equivalent Point
blockIdxCrd :: Int -> Int -> Point
blockIdxCrd b idx = (i, j) where
    (dI, dJ) = divMod (idx - 1) blockWidth
    (i', j') = let (i'', j'') = divMod (b - 1) blockWidth
               in (i'' * 3, j'' * 3)
    (i, j)   = (dI + i' + 1, dJ + j' + 1)

-- | Returns on conflicts relative to a cell
conflicts :: Board -> Cell -> [Point]
conflicts b ((i, j), x) = let
    f = flip canidate False
    (a, z, c) = unitVals b $ pointUnit (i, j) :: ([Int], [Int], [Int])
    (a', z', c') = (f a, f z, f c)
    a_ = g ((i,) <$>) a'
    z_ = g ((,j) <$>) z'
    c_ = g (blockIdxCrd (getBlock (i, j)) <$>) c'
    g u us = join $ flip mapMaybe us \(d, xs) -> if d == x && (not . null) xs
        then Just $ u xs
        else Nothing
    in  a_ <> z_ <> c_

-- | Returns on conflicts on the board
allConflicts :: Board -> [Point]
allConflicts b@(Board g) = Set.toList $ foldr f (Set.empty :: Set.Set Point) $ assocs g where
    f x acc = let cs = Set.fromList $ conflicts b x in Set.union cs acc

-- | view coordinate space as list
cSpace :: [(Int, Int)]
cSpace = flip (,) <$> [1..sudokuSz] <*> [1..sudokuSz]

-- | Return true if the area scanned over could potentially form part of a valid
-- sudoku solution
validateArea :: ScanArea -> Board -> Bool -> Bool
validateArea (row, col, blk) (Board g) z = null . join $ map f r <> map f c <> map f b where
    validate k xs = map (g !) . k <$> xs
    f = flip canidate z
    r = validate rowIndices row
    c = validate colIndices col
    b = validate blockIndices blk

-- | Find all indices in the grid with a given value
findIndices :: Int -> Board -> [(Int, Int)]
findIndices n (Board arr) = map fst . filter (\x -> snd x == n) . assocs $ arr

-- | Produces the list of all values over a list of indices given a generator
-- and seed.
fetch :: Board -> (Int -> [Point]) -> Int -> [Int]
fetch (Board g) f n = (g !) <$> f n

type Prune = (Cell -> Bool)

-- | Creates a function that returns true if a point is in the same row, col, or
-- block as the provided point
pruneDepends :: Cell -> Prune
pruneDepends (c, v) = \(c', v') -> if v == v'
    then fst c == fst c'
        || snd c == snd c'
        || getBlock c == getBlock c'
    else False

-- | Composition of pruning predicates under OR
composePrune :: Prune -> Prune -> Prune
composePrune a b = \x -> b x || a x

-- discards branches of invalid possibilities using prunes. (like a dfs over a
-- multitree)
-- | Generate all possible solutions
forest :: [(Point, [Int])] -> [[Cell]]
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
pointUnit :: Point -> (Int, Int, Int)
pointUnit x = (fst x, snd x, getBlock x)

-- | Takes a list unit indexes (pointUnit) and returns the
-- values of those sectors
unitVals :: Board -> (Int, Int, Int) -> ([Int], [Int], [Int])
unitVals g (a, b, c) = ( fetch g rowIndices a
                       , fetch g colIndices b
                       , fetch g blockIndices c )

-- | Returns all missing values in a sudoku unit vector
missingVals :: ([Int], [Int], [Int]) -> [Int]
missingVals (a, b, c) = Set.toList $ Set.difference full some where
    full = Set.fromList [1 .. sudokuSz]
    some = Set.unions $ Set.fromList <$> [a, b, c]

-- | Returns all possible solutions
solve :: Board -> StdGen -> Maybe [[Cell]]
solve g'@(Board g) rand
    | validateArea scanAll g' False = let
        is = findIndices 0 g'
        -- Shuffling the seed dec time on avg
        f a b = (a, fst $ shuffle' (missingVals b) rand)
        -- Possible solutions at each index
        rs = zipWith f is $ unitVals g' . pointUnit <$> is
        result = if null is
            then (True, [])
            else (False,)  $ do
                sltn <- forest rs :: [[Cell]]
                let test' = g  // sltn
                guard $ validateArea scanAll (Board test') True
                return sltn
        in case result of -- handle no result found case
            (True, x) -> Just x
            (_,  [])  -> Nothing
            (_,  xs)  -> Just xs
    | otherwise = Nothing
