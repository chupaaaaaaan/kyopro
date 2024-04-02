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
import Data.Array.Unboxed
import Data.Array.ST
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
import Data.Foldable

main :: IO ()
main = do
    [h,w,n] <- list1 ucInt
    abcds <- list2 n ucInt

    putStr $ unlines $ map (unwords . map show) $ splitEvery w $ elems $ solve h w abcds

solve :: Int -> Int -> [[Int]] -> UArray (Int, Int) Int
solve h w abcds =
    let origArray = runSTUArray $ do
            as <- newArray ((1,1),(h,w)) 0 :: ST s (STUArray s (Int, Int) Int)
            forM_ abcds $ \[a,b,c,d] -> do
                modifyArray as (a,b) (+1)
                when (c<h) $ modifyArray as (c+1,b) (subtract 1)
                when (d<w) $ modifyArray as (a,d+1) (subtract 1)
                when (c<h && d<w) $ modifyArray as (c+1,d+1) (+1)
            return as

    in runSTUArray $ ascanl2d1 (+) origArray

-- Input
-- converter
type Conv = StateT ByteString Maybe

ucChar :: Conv Char
ucChar = StateT BS.uncons

ucInt :: Conv Int
ucInt = StateT (BS.readInt . BS.dropWhile isSpace)

ucString :: Conv ByteString
ucString = StateT (\bs -> let bs' = BS.dropWhile isSpace bs
                          in if BS.null bs'
                             then Nothing
                             else Just $ BS.break isSpace bs')

-- | read a linear data as List
list1 :: Conv a -> IO [a]
list1 !st = L.unfoldr (runStateT st) <$> BS.getLine

-- | read multi line data as List
list2 :: Int -> Conv a -> IO [[a]]
list2 !n !st = fmap (L.unfoldr (runStateT st)) <$> replicateM n BS.getLine

-- | read a line and convert to Vector
vector1 :: Int -> Conv a -> IO (Vector a)
vector1 n !st = V.unfoldrN n (runStateT st) <$> BS.getLine

toIntGrid :: (Ix a, Ix b) => ((a, b), (a, b)) -> [[Int]] -> UArray (a, b) Int
toIntGrid b = listArray b . L.concat

-- Array
{-# INLINE modifyArray #-}
modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray marr i f = do
  x <- readArray marr i
  writeArray marr i $ f x


-- List
splitEvery :: Int -> [a] -> [[a]]
splitEvery n = L.unfoldr $ \b -> let (e, r) = splitAt n b
                                 in if null e
                                    then Nothing
                                    else Just (e, r)

-- Cumulative Sum
-- | Array用のscanl
ascanl :: (MArray am e1 m, IArray ai e2) => (e1 -> e2 -> e1) -> e1 -> ai Int e2 -> m (am Int e1)
ascanl f a ary = do
    let b@(l,h) = bounds ary

    result <- newArray_ (l-1,h)

    writeArray result (l-1) a

    for_ (range b) $ \i -> do
        x <- readArray result (i-1)
        let y = ary ! i
        writeArray result i (x `f` y)

    return result

-- | Array用のscanl1
ascanl1 :: (MArray am e m, IArray ai e) => (e -> e -> e) -> ai Int e -> m (am Int e)
ascanl1 f ary = do
    let b@(l,h) = bounds ary

    result <- newArray_ (l,h)

    writeArray result l (ary ! l)
    for_ (range (l+1,h)) $ \i -> do
        x <- readArray result (i-1)
        let y = ary ! i
        writeArray result i (x `f` y)

    return result

-- | Array用のscanr
ascanr :: (MArray am e1 m, IArray ai e2) => (e2 -> e1 -> e1) -> e1 -> ai Int e2 -> m (am Int e1)
ascanr f a ary = do
    let b@(l,h) = bounds ary

    result <- newArray_ (l,h+1)

    writeArray result (h+1) a

    for_ (reverse $ range b) $ \i -> do
        let x = ary ! i
        y <- readArray result (i+1)
        writeArray result i (x `f` y)

    return result


-- | Array用のscanr
ascanr1 :: (MArray am e m, IArray ai e) => (e -> e -> e) -> ai Int e -> m (am Int e)
ascanr1 f ary = do
    let b@(l,h) = bounds ary

    result <- newArray_ (l,h)

    writeArray result h (ary ! h)

    for_ (reverse $ range (l,h-1)) $ \i -> do
        let x = ary ! i
        y <- readArray result (i+1)
        writeArray result i (x `f` y)

    return result


-- | 2次元Array用のscanl
ascanl2d :: (MArray am e m, IArray ai e) => (e -> e -> e) -> e -> ai (Int, Int) e -> m (am (Int, Int) e)
ascanl2d f a ary = do
    let ((li,lj),(hi,hj)) = bounds ary
        b = ((li-1,lj-1),(hi,hj))

    result <- newArray b a

    for_ (range $ bounds ary) $ \(i,j) -> 
        writeArray result (i,j) $ ary ! (i,j)

    for_ [(i,j) | i <- [li..hi], j <- [lj..hj]] $ \(i,j) -> do
        x <- readArray result (i-1,j)
        y <- readArray result (i,j)
        writeArray result (i,j) (x `f` y)

    for_ [(i,j) | i <- [li..hi], j <- [lj..hj]] $ \(i,j) -> do
        x <- readArray result (i,j-1)
        y <- readArray result (i,j)
        writeArray result (i,j) (x `f` y)

    return result

-- | 2次元Array用のscanl1
ascanl2d1 :: (MArray ma e m, IArray ia e) => (e -> e -> e) -> ia (Int, Int) e -> m (ma (Int, Int) e)
ascanl2d1 f arr = do
    let b@((li,lj),(hi,hj)) = bounds arr

    result <- newArray_ b

    for_ (range b) $ \(i,j) -> 
        writeArray result (i,j) $ arr ! (i,j)

    for_ [(i,j) | i <- [li+1..hi], j <- [lj..hj]] $ \(i,j) -> do
        x <- readArray result (i-1,j)
        y <- readArray result (i,j)
        writeArray result (i,j) (x `f` y)

    for_ [(i,j) | i <- [li..hi], j <- [lj+1..hj]] $ \(i,j) -> do
        x <- readArray result (i,j-1)
        y <- readArray result (i,j)
        writeArray result (i,j) (x `f` y)

    return result


-- | 2次元累積和から矩形領域の和を計算する際の抽象
rectangleSum :: (Ix a, Ix b, Num a, Num b, Num c) => ((a, b) -> c) -> (a, b) -> (a, b) -> c
rectangleSum f (a,b) (c,d) = f (c,d) + f (a-1,b-1) - f (a-1,d) - f (c,b-1)

-- | 2次元累積和をクエリする
query2dCS :: (IArray a e, Num e) => a (Int, Int) e -> (Int, Int) -> (Int, Int) -> e
query2dCS csArray = rectangleSum (csArray !)
