module My.BinSearch where

import Data.Vector (Vector)
import qualified Data.Vector as V


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

main :: IO ()
main = do
    x <- readLn :: IO Int
    let vecb = V.fromList [1, 2, 3, 4, 4, 4, 4, 5, 5, 6, 6, 6, 6, 7, 8]

    putStrLn "====="
    putStrLn "idx: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14"
    putStrLn "val: 1 2 3 4 4 4 4 5 5 6  6  6  6  7  8"
    putStrLn "====="
    putStrLn $ "LT: " ++ show (bsearch (condLT vecb x) (-1) (V.length vecb))
    putStrLn $ "LE: " ++ show (bsearch (condLE vecb x) (-1) (V.length vecb))
    putStrLn $ "GE: " ++ show (bsearch (condGE vecb x) (V.length vecb) (-1))
    putStrLn $ "GT: " ++ show (bsearch (condGT vecb x) (V.length vecb) (-1))
    putStrLn "====="
