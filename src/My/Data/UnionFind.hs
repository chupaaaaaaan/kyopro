{-# LANGUAGE RecordWildCards #-}
module My.Data.UnionFind where

import Data.Array.MArray
import My.Data.Array
import My.Data.Ref

data UnionFind a r i =
    UF { parentUF :: a i i
       , sizeUF :: a i Int
       , countUF :: r Int
       }

-- | Union-Find木 配列の初期化
ufInit :: (MUArray a i m, MUArray a Int m, Ref r m, Ix i) => (i, i) -> m (UnionFind a r i)
ufInit bnds = UF <$> newUListArray bnds (range bnds) <*> newUArray bnds 1 <*> newRef (rangeSize bnds)

-- | Union-Find木 親を求める
-- parentの要素の値が自分自身と同じ場合、親である
ufRoot :: (MUArray a i m, MUArray a Int m, Ix i) => UnionFind a r i -> i -> m i
ufRoot uf@UF{..} v = do
    p <- readArray parentUF v
    if p == v then return v else do
        q <- ufRoot uf p
        writeArray parentUF v q
        return q

-- | Union-Find木 グループを統合する
ufUnite :: (MUArray a i m, MUArray a Int m, Ref r m, Ix i) => UnionFind a r i -> i -> i -> m ()
ufUnite uf@UF{..} u v = do
    ru <- ufRoot uf u
    rv <- ufRoot uf v
    if ru == rv then return () else do

        modifyRef' countUF (subtract 1)
        
        su <- readArray sizeUF ru
        sv <- readArray sizeUF rv
        if su < sv
            then do writeArray parentUF ru rv
                    writeArray sizeUF rv (su + sv)
            else do writeArray parentUF rv ru
                    writeArray sizeUF ru (su + sv)

-- | Union-Find木 同じグループにいるかクエリする
ufSame :: (MUArray a i m, MUArray a Int m, Ix i) => UnionFind a r i -> i -> i -> m Bool
ufSame uf u v = (==) <$> ufRoot uf u <*> ufRoot uf v

-- | Union-Find木 ある点が属するグループのサイズをクエリする
ufSize :: (MUArray a i m, MUArray a Int m, Ix i) => UnionFind a r i -> i -> m Int
ufSize uf@UF{..} v = ufRoot uf v >>= readArray sizeUF

-- | Union-Find木 連結成分の数をクエリする
ufCount :: Ref r m => UnionFind a r i -> m Int
ufCount UF{..} = readRef countUF
