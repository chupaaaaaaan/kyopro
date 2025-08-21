module My.Algorithm.BinarySearch where

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
