module My.Algorithm.BinarySearch
    ( bsearch
    , bsearchF
    , condGT
    , condGE
    , condLE
    , condLT
    ) where

import Data.Vector.Unboxed qualified as VU

-- | 整数区間に対する二分探索
-- 区間は半開区間 [ok,ng) もしくは (ng,ok] とする
-- (参考) https://qiita.com/drken/items/97e37dd6143e33a64c8c
bsearch :: forall a. (Integral a, Num a) =>
    (a -> Bool) -> -- ^ indexに対する述語
    a ->           -- ^ ok: 解が存在するindex
    a ->           -- ^ ng: 解が存在しないindex
    a
bsearch = bsearchBase div 1

-- | 実数区間に対する二分探索
-- >>> bsearchF (10**(-6)) (\x -> (x * (x + 2)) - 20 < (10**(-6))) 0 10
-- 3.5825753211975098
bsearchF :: forall a. RealFrac a =>
    a ->           -- ^ 区間の最小単位
    (a -> Bool) -> -- ^ ある実数に対する述語
    a ->           -- ^ ok: 解が存在する実数
    a ->           -- ^ ng: 解が存在しない実数
    a
bsearchF = bsearchBase (/)

-- | 二分探索の処理の抽象
bsearchBase :: forall a. (Num a, Ord a) =>
    (a -> a -> a) -> -- ^ 区間を半分にする関数
    a ->             -- ^ 区間の最小単位
    (a -> Bool) ->   -- ^ 位置に対する述語
    a ->             -- ^ ok: 解が存在する位置
    a ->             -- ^ ng: 解が存在しない位置
    a
bsearchBase f ep isOk = go
  where
    go :: a -> a -> a
    go ok ng
        | abs (ok - ng) > ep = let mid = (ok + ng) `f` 2
                               in if isOk mid
                                  then go mid ng
                                  else go ok mid
        | otherwise = ok

-- | Vector上の二分探索で使用可能な「超過・未満・以上・以下」の判定条件
    -- >>> let sorted :: Vector Int = U.fromList [3,4,5,5,6,7]
-- >>>     l = U.length sorted
--
-- >>> U.indexed sorted
-- [(0,3),(1,4),(2,5),(3,5),(4,6),(5,7)]
--
-- >>> bsearch (condGT sorted 5) l 0
-- 4
--
-- >>> bsearch (condLT sorted 5) 0 l
-- 1
-- 
-- >>> bsearch (condGE sorted 5) l 0
-- 2
-- 
-- >>> bsearch (condLE sorted 5) 0 l
-- 3
--
condGT,condLT,condGE,condLE :: (Ord a, VU.Unbox a) => VU.Vector a -> a -> Int -> Bool
condGT vec key idx = key < vec VU.! idx
condLT vec key idx = key > vec VU.! idx
condGE vec key idx = key <= vec VU.! idx
condLE vec key idx = key >= vec VU.! idx
