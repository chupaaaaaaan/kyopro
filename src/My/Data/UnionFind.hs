module My.Data.UnionFind where

import Control.Monad.Reader
import Data.Array.MArray
import My.Data.Array
import My.Data.Ref

data UnionFind a r i =
    UF { parentUF :: a i i
       , sizeUF :: a i Int
       , countUF :: r Int
       }

type RUF a r i m = ReaderT (UnionFind a r i) m

runUF :: (MUArray a i m, MUArray a Int m, Ref r m, Ix i) => (i, i) -> RUF a r i m t -> m t
runUF bnds act = do
    uf <- ufInit bnds
    runReaderT act uf

-- | Union-Find木 配列の初期化
ufInit :: (MUArray a i m, MUArray a Int m, Ref r m, Ix i) => (i, i) -> m (UnionFind a r i)
ufInit bnds = UF <$> newUListArray bnds (range bnds) <*> newUArray bnds 1 <*> newRef (rangeSize bnds)

-- | Union-Find木 親を求める
-- parentの要素の値が自分自身と同じ場合、親である
ufRoot :: (MUArray a i m, MUArray a Int m, Ix i) => i -> RUF a r i m i
ufRoot v = do
    parent <- asks parentUF
    p <- lift (readArray parent v)
    if p == v then return v else do
        q <- ufRoot p
        lift $ writeArray parent v q
        return q

-- | Union-Find木 グループを統合する
ufUnite :: (MUArray a i m, MUArray a Int m, Ref r m, Ix i) => i -> i -> RUF a r i m ()
ufUnite u v = do
    ru <- ufRoot u
    rv <- ufRoot v
    if ru == rv then return () else do

        count <- asks countUF
        lift $ modifyRef' count (subtract 1)
        
        size <- asks sizeUF
        parent <- asks parentUF
        su <- lift $ readArray size ru
        sv <- lift $ readArray size rv
        if su < sv
            then do lift $ writeArray parent ru rv
                    lift $ writeArray size rv (su + sv)
            else do lift $ writeArray parent rv ru
                    lift $ writeArray size ru (su + sv)

-- | Union-Find木 同じグループにいるかクエリする
ufSame :: (MUArray a i m, MUArray a Int m, Ix i) => i -> i -> RUF a r i m Bool
ufSame u v = (==) <$> ufRoot u <*> ufRoot v

-- | Union-Find木 ある点が属するグループのサイズをクエリする
ufSize :: (MUArray a i m, MUArray a Int m, Ix i) => i -> RUF a r i m Int
ufSize v = do
    size <- asks sizeUF
    ufRoot v >>= (lift . readArray size)

-- | Union-Find木 連結成分の数をクエリする
ufCount :: Ref r m => RUF a r i m Int
ufCount = asks countUF >>= lift . readRef
