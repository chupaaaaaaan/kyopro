{-# LANGUAGE LambdaCase #-}
module My.Data.UnionFind where

import Data.Array.MArray
import My.Data.Array
import Control.Monad.Reader

data UnionFind a i =
    UF { parentUF :: a i i
       , sizeUF :: a i Int
       }

type RUF a i m = ReaderT (UnionFind a i) m

runUF :: (MUArray a i m, MUArray a Int m, Ix i) => (i, i) -> RUF a i m t -> m t
runUF bnds act = do
    uf <- ufInit bnds
    runReaderT act uf

-- | Union-Find木 配列の初期化
ufInit :: (MUArray a i m, MUArray a Int m, Ix i) => (i, i) -> m (UnionFind a i)
ufInit bnds = UF <$> newUListArray bnds (range bnds) <*> newUArray bnds 1

-- | Union-Find木 親を求める
-- parentの要素のうち、Nothingのものが親となる
ufRoot :: (MUArray a i m, MUArray a Int m, Ix i) => i -> RUF a i m i
ufRoot v = do
    uf <- ask
    let parent = parentUF uf
    p <- lift (readArray parent v)
    if p == v then return v else do
        q <- ufRoot p
        lift $ writeArray parent v q
        return q

-- | Union-Find木 グループを統合する
ufUnite :: (MUArray a i m, MUArray a Int m, Ix i) => i -> i -> RUF a i m ()
ufUnite u v = do
    uf <- ask
    ru <- ufRoot u
    rv <- ufRoot v
    if ru == rv then return () else do
        let size = sizeUF uf
            parent = parentUF uf
        su <- lift $ readArray size ru
        sv <- lift $ readArray size rv
        if su < sv
            then do lift $ writeArray parent ru rv
                    lift $ writeArray size rv (su + sv)
            else do lift $ writeArray parent rv ru
                    lift $ writeArray size ru (su + sv)

-- | Union-Find木 同じグループにいるかクエリする
ufSame :: (MUArray a i m, MUArray a Int m, Ix i) => i -> i -> RUF a i m Bool
ufSame u v = (==) <$> ufRoot u <*> ufRoot v

-- | Union-Find木 ある点が属するグループのサイズを求める
ufSize :: (MUArray a i m, MUArray a Int m, Ix i) => i -> RUF a i m Int
ufSize v = do
    size <- asks sizeUF
    ufRoot v >>= (lift . readArray size)
