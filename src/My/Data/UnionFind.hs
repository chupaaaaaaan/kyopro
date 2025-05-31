module My.Data.UnionFind
    ( ufInit
    , ufUnite
    , ufSame ) where

import Data.Array.MArray
import My.Data.Array

-- | Union-Find木 配列の初期化
ufInit :: (MBArray a (Maybe i) m, MUArray b Int m, Ix i) => (i, i) -> m (a i (Maybe i), b i Int)
ufInit r = do
    parent <- newBArray r Nothing
    size <- newUArray r 1
    return (parent, size)

-- | Union-Find木 親を求める
-- parentの要素のうち、Nothingのものが親となる
ufRoot :: (MBArray a (Maybe i) m, Ix i) => a i (Maybe i) -> i -> m i
ufRoot parent v = do
    p <- readArray parent v
    case p of
        Nothing -> return v
        Just p' -> do
            q <- ufRoot parent p'
            writeArray parent v (Just q)
            return q

-- | Union-Find木 グループを統合する
ufUnite :: (MBArray a (Maybe i) m, MUArray b Int m, Ix i) => b i Int -> a i (Maybe i) -> i -> i -> m ()
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
ufSame :: (MBArray a (Maybe i) m, Ix i) => a i (Maybe i) -> i -> i -> m Bool
ufSame parent u v = (==) <$> ufRoot parent u <*> ufRoot parent v

