{-# LANGUAGE LambdaCase #-}
module My.Data.UnionFind where

import Data.Array.MArray
import My.Data.Array
import Control.Monad.Reader

data UnionFind a b i =
    UF { parentUF :: a i (Maybe i)
       , sizeUF :: b i Int
       }

type RUF a b i m = ReaderT (UnionFind a b i) m

runUF :: (MBArray a (Maybe i) m, MUArray b Int m, Ix i) => (i, i) -> RUF a b i m t -> m t
runUF bnds act = do
    uf <- ufInit bnds
    runReaderT act uf

-- | Union-Find木 配列の初期化
ufInit :: (MBArray a (Maybe i) m, MUArray b Int m, Ix i) => (i, i) -> m (UnionFind a b i)
ufInit bnds = UF <$> newBArray bnds Nothing <*> newUArray bnds 1

-- | Union-Find木 親を求める
-- parentの要素のうち、Nothingのものが親となる
ufRoot :: (MBArray a (Maybe i) m, MUArray b Int m, Ix i) => i -> RUF a b i m i
ufRoot v = do
    uf <- ask
    let parent = parentUF uf
    lift (readArray parent v) >>= \case
        Nothing -> return v
        Just u -> do
            q <- ufRoot u
            lift $ writeArray parent v (Just q)
            return q

-- | Union-Find木 グループを統合する
ufUnite :: (MBArray a (Maybe i) m, MUArray b Int m, Ix i) => i -> i -> RUF a b i m ()
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
            then do lift $ writeArray parent ru (Just rv)
                    lift $ writeArray size rv (su + sv)
            else do lift $ writeArray parent rv (Just ru)
                    lift $ writeArray size ru (su + sv)

-- | Union-Find木 同じグループにいるかクエリする
ufSame :: (MBArray a (Maybe i) m, MUArray b Int m, Ix i) => i -> i -> RUF a b i m Bool
ufSame u v = (==) <$> ufRoot u <*> ufRoot v

-- | Union-Find木 ある点が属するグループのサイズを求める
ufSize :: (MBArray a (Maybe i) m, MUArray b Int m, Ix i) => i -> RUF a b i m Int
ufSize v = do
    size <- asks sizeUF
    root <- ufRoot v
    lift $ readArray size root
