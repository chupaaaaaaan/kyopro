module My.Algorithm.BinarySearch where

import qualified Data.Vector.Generic as VG

-- | 整数区間に対する二分探索
-- 区間は半開区間 [ok,ng) もしくは (ng,ok] とする
-- (参考) https://qiita.com/drken/items/97e37dd6143e33a64c8c
bsearch :: (Integral a, Num a) =>
    (a -> Bool) -> -- ^ indexに対する述語
    a ->           -- ^ ok: 解が存在するindex
    a ->           -- ^ ng: 解が存在しないindex
    a
bsearch = bsearchBase div 1

-- | 実数区間に対する二分探索
-- >>> bsearchF (10**(-6)) (\x -> (x * (x + 2)) - 20 < (10**(-6))) 0 10
-- 3.5825753211975098
bsearchF :: RealFrac a =>
    a ->           -- ^ 区間の最小単位
    (a -> Bool) -> -- ^ ある実数に対する述語
    a ->           -- ^ ok: 解が存在する実数
    a ->           -- ^ ng: 解が存在しない実数
    a
bsearchF = bsearchBase (/)

-- | 二分探索の処理の抽象
bsearchBase :: (Num a, Ord a) =>
    (a -> a -> a) -> -- ^ 区間を半分にする関数
    a ->             -- ^ 区間の最小単位
    (a -> Bool) ->   -- ^ 位置に対する述語
    a ->             -- ^ ok: 解が存在する位置
    a ->             -- ^ ng: 解が存在しない位置
    a
bsearchBase f ep isOk = go
  where
    go ok ng
        | abs (ok - ng) > ep = let mid = (ok + ng) `f` 2
                               in if isOk mid
                                  then go mid ng
                                  else go ok mid
        | otherwise = ok

-- | Vector上の二分探索で使用可能な「超過・未満・以上・以下」の判定条件
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let sorted :: VU.Vector Int = VU.fromList [3,4,5,5,6,7]
-- >>>     l = VU.length sorted
--
-- >>> VU.indexed sorted
-- [(0,3),(1,4),(2,5),(3,5),(4,6),(5,7)]
--
-- >>> bsearch (sorted`igt`5) l 0
-- 4
--
-- >>> bsearch (sorted`ilt`5) 0 l
-- 1
-- 
-- >>> bsearch (sorted`ige`5) l 0
-- 2
-- 
-- >>> bsearch (sorted`ile`5) 0 l
-- 3
--
igt,ilt,ige,ile :: (Ord a, VG.Vector v a) => v a -> a -> Int -> Bool
(vec `igt` key) idx = vec VG.! idx > key
(vec `ilt` key) idx = vec VG.! idx < key
(vec `ige` key) idx = vec VG.! idx >= key
(vec `ile` key) idx = vec VG.! idx <= key
