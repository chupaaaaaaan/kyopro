{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FunctionalDependencies #-}

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Array.IO
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char
import Data.Foldable
import Data.IORef
import Data.IntPSQ (IntPSQ)
import Data.IntPSQ qualified as PSQ
import Data.List qualified as L
import Data.Maybe
import Data.STRef
import Data.Sequence (Seq(Empty, (:<|), (:|>)), (<|), (|>), (><), ViewL(EmptyL, (:<)), viewl, ViewR(EmptyR, (:>)), viewr)
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
    as <- list1 ucInt

    return ()

-- Input
-- converter
type Conv = StateT ByteString Maybe

ucChar :: Conv Char
ucChar = StateT BS.uncons

ucInt :: Conv Int
ucInt = StateT (BS.readInt . BS.dropWhile isSpace)

ucBS :: Conv ByteString
ucBS = StateT (\bs -> let bs' = BS.dropWhile isSpace bs
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

to1 :: [a] -> a
to1 [a] = a
to1 _ = error "invalid length."

to2 :: [a] -> (a,a)
to2 [a,b] = (a,b)
to2 _ = error "invalid length."

to3 :: [a] -> (a,a,a)
to3 [a,b,c] = (a,b,c)
to3 _ = error "invalid length."

to4 :: [a] -> (a,a,a,a)
to4 [a,b,c,d] = (a,b,c,d)
to4 _ = error "invalid length."

-- Debug
traceWith :: (a -> String) -> a -> a
traceWith f a = trace (f a) a

traceShowWith :: Show b => (a -> b) -> a -> a
traceShowWith f = traceWith (show . f)

traceWithPrefix :: Show a => String -> a -> a
traceWithPrefix prefix = traceWith (\x -> prefix <> show x)

-- Pair
pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) = pair (f . fst, g . snd)

-- Ref
class Monad m => Ref r m | m -> r where
    newRef :: a -> m (r a)
    readRef :: r a -> m a
    writeRef :: r a -> a -> m ()
    modifyRef :: r a -> (a -> a) -> m ()
    modifyRef' :: r a -> (a -> a) -> m ()

instance Ref IORef IO where
    newRef = newIORef
    readRef = readIORef
    writeRef = writeIORef
    modifyRef = modifyIORef
    modifyRef' = modifyIORef'

instance Ref (STRef s) (ST s) where
    newRef = newSTRef
    readRef = readSTRef
    writeRef = writeSTRef
    modifyRef = modifySTRef
    modifyRef' = modifySTRef'

-- Array
{-# INLINE modifyArray #-}
modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray marr i f = readArray marr i >>= writeArray marr i . f

-- Vector
{-# INLINE vSort #-}
vSort :: Ord a => Vector a -> Vector a
vSort = vSortBy compare

{-# INLINE vSortBy #-}
vSortBy :: Ord a => Comparison a -> Vector a -> Vector a
vSortBy f v = V.create $ do
    mv <- V.thaw v
    sortBy f mv
    return mv

-- | ある数nに含まれる指定した素因数bの冪を求める
powerOf :: Int -> Int -> Int
powerOf b n = bsearch (\i -> n`mod`(b^i) == 0) 0 (ceiling $ logBase (fromIntegral b) $ fromIntegral (n+1))

-- | ある数に含まれる2の冪を求める
powerOf2 :: Int -> Int
powerOf2 = powerOf 2

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

-- 二分探索
-- | 整数区間に対する二分探索（参考URL https://qiita.com/drken/items/97e37dd6143e33a64c8c ）
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

-- | 実数区間に対する二分探索
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

-- | 二分探索の処理の抽象
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

-- | Vector上の二分探索で使用可能な「超過・未満・以上・以下」の判定条件
condGT,condLT,condGE,condLE :: Ord a => Vector a -> a -> Int -> Bool
condGT vec key idx = key < vec V.! idx
condLT vec key idx = key > vec V.! idx
condGE vec key idx = key <= vec V.! idx
condLE vec key idx = key >= vec V.! idx

-- グラフ生成
type Graph i = Array i [i]
type WGraph i a = Array i [(i, a)]

-- | 隣接リスト形式の重み付きグラフを生成する
-- 頂点に重みがある場合は、abc 138 dなど参照（https://atcoder.jp/contests/abc138/submissions/15808936 ）
genWeightedGraph, genWeightedDigraph :: Ix i =>
    -- | 頂点の範囲
    (i, i) ->
    -- | 辺のリスト (開始, 終了, 重み)
    [(i, i, a)] ->
    WGraph i a
-- | 無向グラフ
genWeightedGraph b edges = runSTArray $ do
    g <- newArray b []
    forM_ edges $ \(f,t,w) -> do
        modifyArray g f ((t,w):)
        modifyArray g t ((f,w):)
    return g
-- | 有向グラフ
genWeightedDigraph b edges = runSTArray $ do
    g <- newArray b []
    forM_ edges $ \(f,t,w) -> modifyArray g f ((t,w):)
    return g

-- | 隣接リスト形式の重みなしグラフを生成する
genGraph, genDigraph :: Ix i =>
    -- | 頂点の範囲
    (i, i) ->
    -- | 辺のリスト (開始, 終了)
    [(i, i)] ->
    Graph i
-- | 無向グラフ
genGraph b edges = runSTArray $ do
    g <- newArray b []
    forM_ edges $ \(f,t) -> do
        modifyArray g f (t:)
        modifyArray g t (f:)
    return g
-- | 有向グラフ
genDigraph b edges = runSTArray $ do
    g <- newArray b []
    forM_ edges $ \(f,t) -> modifyArray g f (t:)
    return g

-- | 範囲外の点は除外して、ある点に隣接する点を列挙する。
arounds :: Ix i => (i, i) -> (i -> [i]) -> i -> [i]
arounds b neis = filter (b`inRange`) . neis

-- | 2次元グリッドにおける上下左右4点、斜めも加えて8点を列挙する
nei4, nei8 :: (Ix i, Num i) => (i, i) -> [(i, i)]
nei4 (i, j) = [(i+1,j), (i-1,j), (i,j+1), (i,j-1)]
nei8 (i, j) = [(i+1,j), (i-1,j), (i,j+1), (i,j-1), (i+1,j+1), (i+1,j-1), (i-1,j+1), (i-1,j-1)]

-- | 幅優先探索
-- ex. onGraph: bfs (graph !) (bounds graph) dist 1
-- ex. onGrid:  bfs (filter (\v -> grid ! v /= '#') . arounds b nei4) b dist (1,1)
bfs :: forall a m i. (MArray a Int m, Ix i) =>
    -- | 現在点から探索候補点を取得
    (i -> [i]) ->
    -- | 探索範囲のbound
    (i, i) ->
    -- | 開始点からの距離のarray
    a i Int ->
    -- | 開始点
    i ->
    m ()
bfs nexts b dist start = go dist (Seq.singleton start)
    where
        go :: a i Int -> Seq i -> m ()
        go dist queue = case viewl queue of
            -- キューが空であれば探索終了
            EmptyL -> return ()
            -- BFS用のキューから次の探索点を取り出す
            v :< rest -> do
                -- 開始点から探索点までの距離を取得
                d <- readArray dist v
                -- 探索候補点のうち、まだ訪れていない点を列挙
                candidates <- filterM (fmap (== (-1)) . readArray dist) . nexts $ v
                -- 探索候補点に、距離d+1を設定する
                forM_ candidates $ \cand -> writeArray dist cand (d+1)
                -- 探索候補点をキューの末尾に追加し、次の探索へ
                go dist $ rest >< Seq.fromList candidates

-- | 深さ優先探索
-- ex. onGraph: dfs (graph !) (bounds graph) seen 1
-- ex. onGrid:  dfs (filter (\v -> grid ! v /= '#') . arounds b nei4) b seen (1,1)
dfs :: forall a e m i. (MArray a (Maybe i) m, Ix i) =>
    -- | 現在点から探索候補点を取得
    (i -> [i]) ->
    -- | 探索範囲のbound
    (i, i) ->
    -- | 訪問済みかを管理するarray
    a i (Maybe i) ->
    -- | 開始点
    i ->
    m ()
dfs nexts b seen start = go seen start
    where
        go :: a i (Maybe i) -> i -> m ()
        go seen v = do
            s <- readArray seen v
            case s of
                Just _ -> return ()
                Nothing -> do
                    -- 行きがけ順の処理
                    writeArray seen v (Just start)
                    forM_ (nexts v) $ \nv -> do
                        -- 隣接ノード毎の行きがけ順の処理
                        go seen nv
                        -- 隣接ノード毎の帰りがけ順の処理
                    -- 帰りがけ順の処理

-- | ダイクストラ法による最短経路探索
dijkstra :: forall a m . (MArray a Int m, MArray a Bool m) =>
    -- | 隣接リスト形式の重みつきグラフ
    WGraph Int Int ->
    -- | 開始頂点
    Int ->
    m (a Int Bool, a Int Int)
dijkstra graph v = do

    -- 確定済みか否かを保持する配列
    fixed <- newArray (bounds graph) False
    -- 現時点の距離の最小値を保持する配列
    curr <- newArray (bounds graph) maxBound

    -- 開始地点の距離を設定
    writeArray curr v 0

    -- 開始点と距離を優先度付きキュー(PSQ)に入れて探索開始
    go fixed curr $ PSQ.singleton v 0 ()
    return (fixed, curr)

  where
      go :: a Int Bool -> a Int Int -> IntPSQ Int () -> m ()
      go fixed curr psq  = do
          case PSQ.findMin psq of
              -- PSQに何も残っていなければ探索終了
              Nothing -> return ()
              Just (i,c,_) -> do
                  f <- readArray fixed i
                  -- 確定済みであれば、PSQから最小値を取り除き次の計算へ
                  if f then go fixed curr (PSQ.deleteMin psq) else do
                      -- 点を確定済みにマークする
                      writeArray fixed i True
                      -- 隣の点について、距離を更新し、優先度付きキューに追加
                      newPsq <- fmap (insertList psq) $ forM (graph ! i) $ \(nv,nw) -> do
                          cw <- readArray curr nv
                          let new = min cw (c+nw)
                          writeArray curr nv new
                          return (nv,new,())
                      -- 次の計算へ
                      go fixed curr newPsq
      insertList :: Ord p => IntPSQ p v -> [(Int, p, v)] -> IntPSQ p v
      insertList = foldr $ \(i,p,l) q -> PSQ.insert i p l q

-- | Union-Find木 親を求める
ufRoot :: forall a m i. (MArray a (Maybe i) m, Ix i) => a i (Maybe i) -> i -> m i
ufRoot parent v = do
    p <- readArray parent v
    case p of
        Nothing -> return v
        Just p' -> do
            q <- ufRoot parent p'
            writeArray parent v (Just q)
            return q

-- | Union-Find木 グループを統合する
ufUnite :: forall a m i. (MArray a (Maybe i) m, MArray a Int m, Ix i) => a i Int -> a i (Maybe i) -> i -> i -> m ()
ufUnite size parent u v = do
    rootU <- ufRoot parent u
    rootV <- ufRoot parent v
    if rootU == rootV then return () else do
        sizeU <- readArray size rootU
        sizeV <- readArray size rootV
        if sizeU < sizeV
            then do writeArray parent rootU (Just rootV)
                    writeArray size rootV (sizeU + sizeV)
            else do writeArray parent rootV (Just rootU)
                    writeArray size rootU (sizeU + sizeV)

-- | Union-Find木 同じグループにいるかクエリする
ufSame :: forall a m i. (MArray a (Maybe i) m, Ix i) => a i (Maybe i) -> i -> i -> m Bool
ufSame parent u v = do
    rootU <- ufRoot parent u
    rootV <- ufRoot parent v
    return $ rootU == rootV
