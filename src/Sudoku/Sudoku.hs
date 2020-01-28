{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sudoku.Sudoku where
import Data.Array.Unboxed
import Data.Array.ST
import Data.Function (fix)
import Data.List (splitAt)
import qualified  Data.Set as Set
import Data.STRef

import Control.Monad (guard, forM, liftM)
import Control.Monad.ST

import System.Random

data Ennead = Row | Col | Block

type Grid = UArray (Int, Int) Int -- +-+-+-+
                                  -- |1|2|3|
                                  -- +-----+
                                  -- |4|5|6|
                                  -- +-----+
                                  -- |7|8|9|
                                  -- +-+-+-+

type ScanArea = ([Int], [Int], [Int]) -- Rows to scan, colums to scan, blocks to scan

type Possibility = ((Int, Int), Int)

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
canidate :: Int -> [Int] -> Bool -> Bool
canidate mx xs z = runST do
    -- init array of false at list sz
    arr <- newArray (1, mx) False :: ST s (STUArray s Int Bool)
    i <- newSTRef xs -- list iterator
    fix $ \go -> do
        r <- readSTRef i -- iterate
        case r of
            []     -> return True -- if iterator empty end loop
            (0:us) -> do -- 0 = unfilled
                if z
                    then return False
                    else writeSTRef i us >> go -- inc iterator
            (u:us) -> do -- else uncons for iteration
                tmp <- not <$> readArray arr u -- if true implies duplication
                writeSTRef i us -- inc iterator
                writeArray arr u True -- mark value as exhausted
                if tmp then go else return False

validateArea :: ScanArea -> Grid -> Bool -> Bool
validateArea (row, col, blk) g z = and $ map f r <> map f c <> map f b where
    validate k xs = map (g !) . k <$> xs
    f = flip (canidate sudokuSz) z
    r = validate rowIndices row
    c = validate colIndices col
    b = validate blockIndices blk

findIndices :: Int -> UArray (Int, Int) Int -> [(Int, Int)]
findIndices n arr = map fst . filter (\x -> snd x == n) . assocs $ arr

fetch :: Grid -> (Int -> [(Int, Int)]) -> Int -> [Int]
fetch g f n = (g !) <$> f n

shuffle' :: [a] -> StdGen -> ([a],StdGen)
shuffle' xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray' n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
    where
        n = length xs
        newArray' :: Int -> [a] -> ST s (STArray s Int a)
        newArray' n xs =  newListArray (1,n) xs

-- returns
depends :: (Int, Int) -> (Int, Int) -> [(Ennead, Int)]
depends a b = f fst Row
    <> f snd Col
    <> f getBlock Block
    where
        f g t = let
            a' = g a
            b' = g b
            in if a' == b'
                then [(t, a')]
                else []

type Prune = (Possibility -> Bool)

pruneDepends :: Possibility -> Prune
pruneDepends (c, v) = \(c', v') -> if v == v'
    then fst c == fst c'
        || snd c == snd c'
        || getBlock c == getBlock c'
    else False

composePrune :: Prune -> Prune -> Prune
composePrune a b = \x -> b x || a x

forest :: [((Int, Int), [Int])] -> [[Possibility]]
forest [] = []
forest ((c', vs') : xs') = map (map fst) $ f xs' [[((c', v'), pruneDepends (c', v'))] | v' <- vs'] where
    f :: [((Int, Int), [Int])] -> [[(Possibility, Prune)]] -> [[(Possibility, Prune)]]
    f [] acc = acc
    f ((c, vs) : xs) acc = f xs [ g (c, v) u x | (x@((_,u):_)) <- acc, v <- vs]
    g p u xs = if u p
        then []
        else (p, composePrune (pruneDepends p) u) : xs

solve :: Grid -> StdGen -> Maybe [Possibility]
solve g rand
    | validateArea scanAll g False = let
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
        rs = zipWith (\a b -> (a, fst $ shuffle' (k b) rand)) is vs -- Possible solutions at each index
        in if null is
            then Just []
            else Just . head $ do
                sltn <- forest rs :: [[Possibility]]
                let test = g  // sltn
                guard $ validateArea scanAll test True
                return sltn
    | otherwise = Nothing

chunksOf :: forall a. Int -> [a] -> [[a]]
chunksOf n xs = f xs where
    f []  = []
    f xs' = let
        (h, t) = splitAt n xs'
        in h : f t

main :: IO ()
main = do
    let arr = listArray ((1,1), (9, 9)) test :: UArray (Int, Int) Int
    rand <- newStdGen
    rand' <- newStdGen
    let u = shuffle' (f sudokuSz) rand'
    let arr' = arr // ((take 51 . fst $ u) `zip` repeat 0)
    case solve arr rand of
        Nothing -> putStrLn "no solution found"
        Just x  -> putStrLn . unlines $ map show $ chunksOf sudokuSz $ elems $ arr // x
    where
        f n = (,) <$> [1 .. n] <*> [1 .. n]

-- ALG
-- Validate if the input *could be valid*
-- If *could be valid* then find at each missing cell each value that in (isolation) could be valid
--    in respect to row, column, block
-- Generate a list of all possible outcomes and save only those that are correct
-- DETAILS
-- set of valid insertions at unfilled cell = [1..9] \ (rowV ∪ colV ∪ blockV)
-- FASTER
-- idea 1 - for each new layer of posibilities check if is dependent on parent
-- posibilties. If so check if there is a conflict. If a conflict does exist,
-- prune the new node from the tree.
-- idea 2 - filters!!! Filter at each layer of the bind tree. Filter by row, by
-- colum (easy). filter by block... hard

test :: [Int]
test = [ 0,0,0,0,6,0,0,0,0,
         0,2,0,9,0,4,0,6,0,
         4,0,0,0,7,0,0,0,9,
         0,3,0,0,0,0,0,9,2,
         0,0,9,0,8,0,5,0,0,
         1,0,0,0,0,0,0,3,0,
         5,0,0,0,0,0,0,0,1,
         0,9,0,2,0,6,0,4,0,
         0,0,0,0,5,0,0,0,0 ]
