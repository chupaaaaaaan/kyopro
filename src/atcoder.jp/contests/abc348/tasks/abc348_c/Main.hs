{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.Foldable
import Data.List qualified as L
import Data.Maybe
import Data.STRef
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as VM
import Debug.Trace
import Numeric

main :: IO ()
main = do
    n <- readLn @Int
    print . maximum . map (fst . V.minimumOn fst) . V.groupBy (\p q -> snd p == snd q) . sortedVectorBy (\p q -> compare (snd p) (snd q)) . V.fromList . map (\[a,c] -> (a,c)) =<< list2 n ucInt


sortedVectorBy :: Ord a => Comparison a -> Vector a -> Vector a
sortedVectorBy f v = V.create $ do
    mv <- V.thaw v
    sortBy f mv
    return mv

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
modifyArray marr i f = readArray marr i >>= writeArray marr i . f

-- Vector
{-# INLINE sortedVector #-}
sortedVector :: Ord a => Vector a -> Vector a
sortedVector v = V.create $ do
    mv <- V.thaw v
    sort mv
    return mv

-- Cumulative Sum
-- | Array用のscanl
ascanl :: (MArray ma e1 m, IArray ia e2) => (e1 -> e2 -> e1) -> e1 -> ia Int e2 -> m (ma Int e1)
ascanl f a arr = do
    let b@(l,h) = bounds arr

    result <- newArray_ (l-1,h)

    writeArray result (l-1) a

    for_ (range b) $ \i -> do
        x <- readArray result (i-1)
        let y = arr ! i
        writeArray result i (x `f` y)

    return result

-- | Array用のscanl1
ascanl1 :: (MArray ma e m, IArray ia e) => (e -> e -> e) -> ia Int e -> m (ma Int e)
ascanl1 f arr = do
    let b@(l,h) = bounds arr

    result <- newArray_ b

    writeArray result l (arr ! l)
    for_ (range (l+1,h)) $ \i -> do
        x <- readArray result (i-1)
        let y = arr ! i
        writeArray result i (x `f` y)

    return result

-- | Array用のscanr
ascanr :: (MArray ma e1 m, IArray ia e2) => (e2 -> e1 -> e1) -> e1 -> ia Int e2 -> m (ma Int e1)
ascanr f a arr = do
    let b@(l,h) = bounds arr

    result <- newArray_ (l,h+1)

    writeArray result (h+1) a

    for_ (reverse $ range b) $ \i -> do
        let x = arr ! i
        y <- readArray result (i+1)
        writeArray result i (x `f` y)

    return result


-- | Array用のscanr1
ascanr1 :: (MArray ma e m, IArray ia e) => (e -> e -> e) -> ia Int e -> m (ma Int e)
ascanr1 f arr = do
    let b@(l,h) = bounds arr

    result <- newArray_ b

    writeArray result h (arr ! h)

    for_ (reverse $ range (l,h-1)) $ \i -> do
        let x = arr ! i
        y <- readArray result (i+1)
        writeArray result i (x `f` y)

    return result


-- | 2次元Array用のscanl
ascanl2d :: (MArray ma e m, IArray ia e) => (e -> e -> e) -> e -> ia (Int, Int) e -> m (ma (Int, Int) e)
ascanl2d f a arr = do
    let b@((li,lj),(hi,hj)) = bounds arr
        bx = ((li-1,lj-1),(hi,hj))

    result <- newArray bx a

    for_ (range b) $ \(i,j) ->
        writeArray result (i,j) $ arr ! (i,j)

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

-- | Binary Search
-- https://qiita.com/drken/items/97e37dd6143e33a64c8c
-- 区間は [ok,ng) もしくは (ng,ok] で考える
bsearch :: forall a. (Integral a, Num a) =>
    -- | indexに対する述語
    (a -> Bool) ->
    -- | ok: 解が存在するindex
    a ->
    -- | ng: 解が存在しないindex
    a ->
    a
bsearch = bsearchBase div 1

-- | Binary Search for
-- bsearchの、区間がRealFracとなった版
bsearchF :: forall a. RealFrac a =>
    -- | 区間の最小単位
    a ->
    -- | ある実数に対する述語
    (a -> Bool) ->
    -- | ok: 解が存在する実数
    a ->
    -- | ng: 解が存在しない実数
    a ->
    a
bsearchF = bsearchBase (/)

-- | Binary Search Base
-- 二分探索の処理の抽象
bsearchBase :: forall a. (Num a, Ord a) =>
    -- | 区間を半分にする関数
    (a -> a -> a) ->
    -- | 区間の最小単位
    a ->
    -- | 位置に対する述語
    (a -> Bool) ->
    -- | ok: 解が存在する位置
    a ->
    -- | ng: 解が存在しない位置
    a ->
    a
bsearchBase f ep isOk = go
  where
    go :: a -> a -> a
    go ok ng
        | abs (ok - ng) > ep =
            let mid = (ok + ng) `f` 2
             in if isOk mid
                    then go mid ng
                    else go ok mid
        | otherwise = ok

condGT,condLT,condGE,condLE :: Ord a => Vector a -> a -> Int -> Bool
condGT vec key idx = key < vec V.! idx
condLT vec key idx = key > vec V.! idx
condGE vec key idx = key <= vec V.! idx
condLE vec key idx = key >= vec V.! idx
