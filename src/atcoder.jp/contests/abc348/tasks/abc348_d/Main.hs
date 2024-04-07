{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Sequence (Seq(Empty, (:<|), (:|>)), (<|), (|>), (><), ViewL(EmptyL, (:<)), ViewR(EmptyR, (:>)))
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as VM
import Debug.Trace
import Numeric
import Data.Bifunctor
import Data.IntMap (IntMap)
import Data.Array.IO

main :: IO ()
main = do
    [h,w] <- list1 ucInt
    ag <- toCharGrid ((1,1), (h,w)) <$> list2 h ucChar
    n <- readLn @Int
    rces <- zip [1..] . map to3 <$> list2 n ucInt

    let start = fst . head . filter (\(_,e) -> e == 'S') $ assocs ag
        startIdx = filter (\(_,(r,c,_)) -> r == fst start && c == snd start) rces
        end = fst . head . filter (\(_,e) -> e == 'T') $ assocs ag
        endIdx = filter (\(_,(r,c,_)) -> r == fst end && c == snd end) rces
        graph = if null endIdx
                then genDigraph (n+1) $ mkEdges h w ag ((n+1,(fst end, snd end, 0)) : rces)
                else genDigraph n $ mkEdges h w ag rces

    if null startIdx then putStrLn "No" else do
        seen <- bfs graph [fst . head $ startIdx] :: IO (IOArray Int Int)
        tin <- readArray seen (n+1)

        putStrLn $ if tin /= -1 then "Yes" else "No"


to1 [a] = a
to2 [a,b] = (a,b)
to3 [a,b,c] = (a,b,c)
to4 [a,b,c,d] = (a,b,c,d)


mkEdges :: Int -> Int -> UArray (Int, Int) Char -> [(Int, (Int, Int, Int))] -> [(Int, Int)]
mkEdges h w ag rces = concatMap (go rces) rces
    where go :: [(Int, (Int, Int, Int))] -> (Int, (Int, Int, Int)) -> [(Int, Int)]
          go rcess (i,(ri,ci,ei)) =
              let seen = runSTUArray $ gridBfs ((/='#') . (ag!)) ((1,1),(h,w)) [(ri,ci)]
              in map ((i,) . fst) . filter (\(j,(rj,cj,ej)) -> i /= j && seen ! (rj,cj) <= ei && seen ! (rj,cj) /= -1) $ rcess

-- | あるセルに隣接するセルを列挙する。
-- 範囲外のセルは除外する。
arounds :: Ix i => (i, i) -> (i -> [i]) -> i -> [i]
arounds b neis ij = filter (b`inRange`) $ neis ij

nei4, nei8 :: (Ix i, Ix j, Num i, Num j) => (i, j) -> [(i, j)]
nei4 (i, j) = [(i+1,j), (i-1,j), (i,j+1), (i,j-1)]
nei8 (i, j) = [(i+1,j), (i-1,j), (i,j+1), (i,j-1), (i+1,j+1), (i+1,j-1), (i-1,j+1), (i-1,j-1)]


-- | グラフ上での幅優先探索
bfs :: forall a m w. MArray a Int m =>
    -- | 隣接リスト形式で表現したグラフ
    Array Int [Int] ->
    -- | 開始頂点のリスト
    [Int] ->
    m (a Int Int)
bfs graph = bfsBase (graph !) (bounds graph)

-- | 2次元グリッド上での幅優先探索
gridBfs :: forall a m. MArray a Int m =>
    -- | 探索対象のセルの判定
    ((Int, Int) -> Bool) ->
    -- | 2次元グリッドのbound
    ((Int, Int), (Int, Int)) ->
    -- | 開始セルのリスト
    [(Int, Int)] ->
    m (a (Int, Int) Int)
gridBfs isCand b = bfsBase (filter isCand . arounds b nei4) b

-- | 幅優先探索の抽象
bfsBase :: forall a m i. (MArray a Int m, Ix i) =>
    -- | 隣接セルへの拡張
    (i -> [i]) ->
    -- | 探索領域のbound
    (i, i) ->
    -- | 開始セルのリスト
    [i] ->
    m (a i Int)
bfsBase expandCandidate b starts = do

    -- 開始セルからの距離
    -- '-1'は、そのセルを訪れていないことを表す。
    dist <- newArray b (-1)

    -- 開始セルには距離0を設定する
    forM_ starts $ \start -> writeArray dist start 0

    -- 開始セルをキューに入れて探索開始
    go dist (Seq.fromList starts)

    return dist

    where
        go :: a i Int -> Seq i -> m ()
        go dist queue
            | Seq.null queue = return ()
            | otherwise = do
                  -- BFS用のキューから次の探索セルを取り出す
                  let (idx :< qRest) = Seq.viewl queue
                  -- 探索セルの距離を返却
                  d <- readArray dist idx
                  -- 探索候補となるセルを列挙
                  candidates <- filterM (fmap (== (-1)) . readArray dist) . expandCandidate $ idx
                  -- 探索候補のセルに、距離d+1を設定する
                  forM_ candidates $ \cand -> writeArray dist cand (d+1)
                  -- 探索候補のセルをキューの末尾に追加し、次の探索へ
                  go dist $ qRest >< Seq.fromList candidates

-- | 重みなしの隣接リスト形式の有向グラフを生成する
genDigraph ::
    -- | 頂点の個数
    Int ->
    -- | 辺のリスト (開始, 終了)
    [(Int, Int)] ->
    Array Int [Int]
genDigraph n edges = runSTArray $ do
    g <- newArray (1,n) []
    forM_ edges $ \(f,t) -> modifyArray g f (t:)
    return g




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

toCharGrid :: (Ix a, Ix b) => ((a, b), (a, b)) -> [[Char]] -> UArray (a, b) Char
toCharGrid b = listArray b . L.concat

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
