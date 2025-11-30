{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module My.Data.SegTree where

import Control.Monad.Fix
import Control.Monad.Primitive
import Data.Bits
import Data.Foldable
import qualified Data.Vector.Unboxing as VU
import qualified Data.Vector.Unboxing.Mutable as VUM
import My.Math.Util

data SegTree s e =
    SgT { sizeSgT :: Int
        , nSgT :: Int
        , segtree :: VUM.MVector s e
        }

type SegTreeIO = SegTree (PrimState IO)

sgtInit :: (Monoid e, VUM.Unboxable e, PrimMonad m) => Int -> e -> m (SegTree (PrimState m) e)
sgtInit n v = let size = bitceil n
              in SgT size n <$> VUM.replicate (size*2) v

sgtBuild :: (Monoid e, VUM.Unboxable e, PrimMonad m) => VU.Vector e -> m (SegTree (PrimState m) e)
sgtBuild iv = do
    let il = VU.length iv
        size = bitceil il

    mv <- VUM.replicate (size*2) mempty
    for_ [0..il-1] $ \p -> VUM.write mv (size+p) (iv VU.! p)
    for_ [size-1,size-2..1] $ sgtInnerUpdate mv

    return $ SgT size il mv

sgtWrite :: (Monoid e, VUM.Unboxable e, PrimMonad m) => SegTree (PrimState m) e -> Int -> e -> m ()
sgtWrite SgT{..} p v = do
    let pos = sizeSgT + p

    VUM.write segtree pos v
    flip fix pos $ \loop i -> if i <= 1 then return () else do
                     let parent = i .>>. 1
                     sgtInnerUpdate segtree parent
                     loop parent

sgtModify :: (Monoid e, VUM.Unboxable e, PrimMonad m) => SegTree (PrimState m) e -> Int -> e -> m ()
sgtModify SgT{..} p v = do
    let pos = sizeSgT + p

    VUM.modify segtree (v<>) pos
    flip fix pos $ \loop i -> if i <= 1 then return () else do
                     let parent = i .>>. 1
                     sgtInnerUpdate segtree parent
                     loop parent

sgtRead :: (Monoid e, VUM.Unboxable e, PrimMonad m) => SegTree (PrimState m) e -> Int -> m e
sgtRead SgT{..} p = VUM.read segtree (sizeSgT + p)

sgtQuery :: (Monoid e, VUM.Unboxable e, PrimMonad m) => SegTree (PrimState m) e -> Int -> Int -> m e
sgtQuery SgT{..} l r = go (l + sizeSgT) (r + sizeSgT) mempty mempty
    where
        go !iL !iR !smL !smR
            | iL < iR = do
                  smL' <- if iL `testBit` 0
                          then (<> smL) <$> VUM.read segtree iL
                          else return smL
                  smR' <- if iR `testBit` 0
                          then (smR <>) <$> VUM.read segtree (iR-1)
                          else return smR
                  let iL'  = if iL `testBit` 0 then iL+1 else iL
                      iR'  = if iR `testBit` 0 then iR-1 else iR
                  go (iL' .>>. 1) (iR' .>>. 1) smL' smR'
            | otherwise = return $ smL <> smR

sgtInnerUpdate :: (Monoid e, VUM.Unboxable e, PrimMonad m) => VUM.MVector (PrimState m) e -> Int -> m ()
sgtInnerUpdate tree u = do
    x <- VUM.read tree (u .<<. 1)
    y <- VUM.read tree (u .<<. 1 + 1)
    VUM.write tree u (x <> y)

sgtMaxRight :: forall e m. (Monoid e, VUM.Unboxable e, PrimMonad m) => SegTree (PrimState m) e -> Int -> (e -> Bool) -> m Int
sgtMaxRight SgT{..} l f
    | l >= nSgT = return nSgT
    | otherwise = do
          (p, acc) <- ascend (l + sizeSgT) mempty
          if p == 0
              then return nSgT
              else descend p acc

    where  ascend :: Int -> e -> m (Int, e)
           ascend !p !acc = do
               let p' = strip2 p
               accv <- (acc <>) <$> VUM.read segtree p'
               if | not (f accv) -> return (p', acc)
                  | isv2 (p'+1)  -> return (0, accv)
                  | otherwise    -> ascend (p'+1) accv

           descend :: Int -> e -> m Int
           descend !p !acc
               | p >= sizeSgT = return $ p - sizeSgT
               | otherwise = do
                     let p' = p .<<. 1
                     accv <- (acc <>) <$> VUM.read segtree p'
                     if f accv
                         then descend (p'+1) accv
                         else descend p' acc

sgtMinLeft :: forall e m. (Monoid e, VUM.Unboxable e, PrimMonad m) => SegTree (PrimState m) e -> Int -> (e -> Bool) -> m Int
sgtMinLeft SgT{..} r f
    | r <= 0 = return 0
    | otherwise = do
          (p, acc) <- ascend (r - 1 + sizeSgT) mempty
          if p == 0
              then return 0
              else descend p acc

    where  ascend :: Int -> e -> m (Int, e)
           ascend !p !acc = do
               let p' = strip2' p
               accv <- (<> acc) <$> VUM.read segtree p'
               if | not (f accv) -> return (p', acc)
                  | isv2 p'      -> return (0, accv)
                  | otherwise    -> ascend (p'-1) accv

           strip2' :: Int -> Int
           strip2' = go
               where go !i | i > 1 && i `testBit` 0 = go (i .>>. 1)
                           | otherwise = i

           descend :: Int -> e -> m Int
           descend !p !acc
               | p >= sizeSgT = return $ p + 1 - sizeSgT
               | otherwise = do
                     let p' = p .<<. 1
                     accv <- (<> acc) <$> VUM.read segtree (p'+1)
                     if f accv
                         then descend p' accv
                         else descend (p'+1) acc
