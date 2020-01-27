{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module Sudoku.Sudoku where
import Data.Array.Unboxed
import Data.Array.ST
import Data.Function (fix)
import qualified  Data.Set as Set
import Data.STRef

import Control.Monad (guard)
import Control.Monad.ST

data Ennead = Row | Col | Block

type Grid = UArray (Int, Int) Int -- +-+-+-+
                                  -- |1|2|3|
                                  -- +-----+
                                  -- |4|5|6|
                                  -- +-----+
                                  -- |7|8|9|
                                  -- +-+-+-+

type ScanArea = ([Int], [Int], [Int]) -- Rows to scan, colums to scan, blocks to scan
sudokuSz :: Int
sudokuSz = 9

blockWidth :: Int
blockWidth = 3

scanAll :: ScanArea
scanAll = let !r = [1..sudokuSz] in (r, r, r)

getBlock :: (Int, Int) -> Int
getBlock (i, j) = i' + j' where
    i' = (i - 1) `div` blockWidth * blockWidth + 1
    j' = (j - 1) `div` blockWidth

blockIndices :: Int -> [(Int, Int)]
blockIndices n = is `distrubute` js where
    distrubute is js = [ (i, j) | i <- is, j <- js ]
    is = let del = (n - 1) `div` blockWidth  * blockWidth  in [del + 1 .. blockWidth + del]
    js = let del = (n - 1) `mod` blockWidth  * blockWidth  in [del + 1 .. blockWidth + del]

rowIndices :: Int -> [(Int, Int)]
rowIndices i = [(i, j) | j <- [1 .. sudokuSz]]

colIndices :: Int -> [(Int, Int)]
colIndices j = [(i, j) | i <- [1 .. sudokuSz]]

-- returns true if a list of integers > 1 contains no duplicates in linear time
canidate :: Int -> [Int] -> Bool
canidate mx xs = runST do
    -- init array of false at list sz
    arr <- newArray (1, mx) False :: ST s (STUArray s Int Bool)
    i <- newSTRef xs -- list iterator
    fix $ \go -> do
        r <- readSTRef i -- iterate
        case r of
            []     -> return True -- if iterator empty end loop
            (0:us) -> do -- 0 = unfilled
                writeSTRef i us -- inc iterator
                go
            (u:us) -> do -- else uncons for iteration
                tmp <- not <$> readArray arr u -- if true implies duplication
                writeSTRef i us -- inc iterator
                writeArray arr u True -- mark value as exhausted
                if tmp then go else return False


validateArea :: ScanArea -> Grid -> Bool
validateArea (row, col, blk) g  = and $ map f r <> map f c <> map f b where
    validate k xs = map (g !) . k <$> xs
    f = canidate sudokuSz
    r = validate rowIndices row
    c = validate colIndices col
    b = validate blockIndices blk

findIndices :: Int -> UArray (Int, Int) Int -> [(Int, Int)]
findIndices n arr = map fst . filter (\x -> snd x == n) . assocs $ arr

fetch :: Grid -> (Int -> [(Int, Int)]) -> Int -> [Int]
fetch g f n = (g !) <$> f n

forest :: [((Int, Int), [Int])] -> [[((Int, Int), Int)]]
forest [] = []
forest ((c', vs') : xs') = f xs' [[(c', v')] | v' <- vs'] where
    f [] acc = acc
    f ((c, vs) : xs) acc = f xs [(c, v) : x | x <- acc, v <- vs]

solve :: Grid -> Maybe [((Int, Int), Int)]
solve g
    | validateArea scanAll g = let
        is = findIndices 0 g
        search x = (fst x, snd x, getBlock x)
        us = search <$> is
        f (a, b, c) = ( fetch g rowIndices a
                      , fetch g colIndices b
                      , fetch g blockIndices c )
        vs = f <$> us -- (rowV, colV, blockV)
        k (a, b, c) = let
            cmplt = Set.fromList [ 1 .. sudokuSz ]
            us'   = Set.unions $ Set.fromList <$> [a, b, c]
            in Set.toList $ Set.difference cmplt  us'
        rs = zipWith (\a b -> (a, k b)) is vs -- Possible solutions at each index
        in if null is
            then Nothing
            else Just . head $ do
                sltn <- forest rs :: [[((Int, Int), Int)]]
                let test = g  // sltn
                guard $ validateArea scanAll test
                return sltn
    | otherwise = Nothing

main :: IO ()
main = print . solve $ arr where
    arr = listArray ((1,1), (9, 9)) test :: UArray (Int, Int) Int


-- ALG
-- Validate if the input *could be valid*
-- If *could be valid* then find at each missing cell each value that in (isolation) could be valid
--    in respect to row, column, block
-- Generate a list of all possible outcomes and save only those that are correct
-- DETAILS
-- set of valid insertions at unfilled cell = [1..9] \ (rowV ∪ colV ∪ blockV)

test :: [Int]
test = [ 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 8, 0, 0, 0, 0, 4, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 6, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 2, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 2, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 8, 0, 0, 0, 0, 4, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 6, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0
       , 2, 0, 0, 0, 0, 0, 0, 0, 0
       , 0, 0, 0, 0, 0, 0, 2, 0, 0
       , 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
