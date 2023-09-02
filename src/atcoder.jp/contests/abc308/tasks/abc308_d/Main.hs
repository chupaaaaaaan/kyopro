{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Array.IArray
import Data.Array.ST
import Data.Bifunctor
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.List as L
import Data.Maybe
import Data.STRef
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as VM
import Numeric

main :: IO ()
main = do
    [h, w] <- list1 ucInt
    grid <- listArray ((1, 1), (h, w)) . concat <$> list2 h ucChar :: IO (Array (Int, Int) Char)

    putStrLn $ solve h w grid

solve :: Int -> Int -> Array (Int, Int) Char -> String
solve h w grid =
    let distArray = gridDfs ((1, 1), (h, w)) (1, 1) (\d idx -> grid ! idx == "snuke" !! (d `mod` 5)) grid
     in if distArray ! (h, w) == -1 then "No" else "Yes"

data SearchType = DFS | BFS

-- search on grid by dfs or bfs.
searchOnGrid ::
    -- | DFS or BFS
    SearchType -> 
    -- | (top-left corner, bottom-right corner)
    ((Int, Int), (Int, Int)) ->
    -- | starting cell
    (Int, Int) ->
    -- | search condition (distance from starting cell, target cell)
    (Int -> (Int, Int) -> Bool) ->
    -- | grid in searching
    Array (Int, Int) a ->
    Array (Int, Int) Int
searchOnGrid searchType tlbr start predicate grid = runSTArray $ do
    -- distance from start point
    distance <- newArray tlbr (-1)

    when (predicate 0 start) $ do
        writeArray distance start 0
        go distance (Seq.singleton start)

    return distance
        
  where
    go :: STArray s (Int, Int) Int -> Seq.Seq (Int, Int) -> ST s ()
    go distance queue
        | Seq.null queue = return ()
        | otherwise = do
            let (idx Seq.:< q') = Seq.viewl queue
            dist <- readArray distance idx
            candidates <- filterM (readArray distance >=> (return . (== (-1)))) . filter (predicate (dist + 1)) . neighbour4 tlbr $ idx
            forM_ candidates $ \cand -> writeArray distance cand (dist + 1)
            go distance $ case searchType of
                DFS -> Seq.fromList candidates Seq.>< q'
                BFS -> q' Seq.>< Seq.fromList candidates

-- | execute dfs on 2d-grid.
gridDfs = searchOnGrid DFS

-- | execute bfs on 2d-grid.
gridBfs = searchOnGrid BFS

{- | list neighbour cells
 neighbour cells are below.
 o : target cell
 x : neighbour cells

 #.....#
 .     .
 .  x  .
 . xox .
 .  x  .
 .     .
 #.....#
-}
neighbour4 ::
    -- | (top-left corner, bottom-right corner)
    ((Int, Int), (Int, Int)) ->
    -- | target cell
    (Int, Int) ->
    [(Int, Int)]
neighbour4 ((t, l), (b, r)) (i, j) = filter inGrid . map (bimap (i +) (j +)) $ [(-1, 0), (1, 0), (0, -1), (0, 1)]
  where
    inGrid (i, j) = t <= i && i <= b && l <= j && j <= r

-- Input
-- converter
ucChar :: StateT ByteString Maybe Char
ucChar = StateT BS.uncons

ucInt :: StateT ByteString Maybe Int
ucInt = StateT (BS.readInt . BS.dropWhile isSpace)

-- read a linear data as List
list1 :: StateT ByteString Maybe a -> IO [a]
list1 !st = L.unfoldr (runStateT st) <$> BS.getLine

list2 :: Int -> StateT ByteString Maybe a -> IO [[a]]
list2 !n !st = fmap (L.unfoldr (runStateT st)) <$> replicateM n BS.getLine

-- read a column Vector of order n
vec1 :: Int -> StateT ByteString Maybe a -> IO (Vector a)
vec1 !n !st = toVec1 n <$> list2 n st

toVec1 :: Int -> [[a]] -> Vector a
toVec1 !n = V.fromListN n . fmap (\[a] -> a)

-- read a column Vector of order n whose element is 2-tuple
toVect :: Int -> [[a]] -> Vector (a, a)
toVect !n = V.fromList . fmap (\[a, b] -> (a, b))
