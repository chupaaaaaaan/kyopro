module My.Data.UnionFind where

import Data.Array.MArray
import My.Data.Array

data UnionFind a b i =
    UF { parentUF :: a i (Maybe i)
       , sizeUF :: b i Int
       }

-- | Union-Find木 配列の初期化
ufInit :: (MBArray a (Maybe i) m, MUArray b Int m, Ix i) => (i, i) -> m (UnionFind a b i)
ufInit r = UF <$> newBArray r Nothing <*> newUArray r 1

-- | Union-Find木 親を求める
-- parentの要素のうち、Nothingのものが親となる
ufRoot :: (MBArray a (Maybe i) m, MUArray b Int m, Ix i) => UnionFind a b i -> i -> m i
ufRoot uf v = do
    let parent = parentUF uf
    p <- readArray parent v
    case p of
        Nothing -> return v
        Just p' -> do
            q <- ufRoot uf p'
            writeArray parent v (Just q)
            return q

-- | Union-Find木 グループを統合する
ufUnite :: (MBArray a (Maybe i) m, MUArray b Int m, Ix i) => UnionFind a b i -> i -> i -> m ()
ufUnite uf u v = do
    rootU <- ufRoot uf u
    rootV <- ufRoot uf v
    if rootU == rootV then return () else do
        let size = sizeUF uf
            parent = parentUF uf
        sizeU <- readArray size rootU
        sizeV <- readArray size rootV
        if sizeU < sizeV
            then do writeArray parent rootU (Just rootV)
                    writeArray size rootV (sizeU + sizeV)
            else do writeArray parent rootV (Just rootU)
                    writeArray size rootU (sizeU + sizeV)

-- | Union-Find木 同じグループにいるかクエリする
ufSame :: (MBArray a (Maybe i) m, MUArray b Int m, Ix i) => UnionFind a b i -> i -> i -> m Bool
ufSame uf u v = (==) <$> ufRoot uf u <*> ufRoot uf v

-- | Union-Find木 ある点が属するグループのサイズを求める
ufSize :: (MBArray a (Maybe i) m, MUArray b Int m, Ix i) => UnionFind a b i -> i -> m Int
ufSize uf v = ufRoot uf v >>= readArray (sizeUF uf)
