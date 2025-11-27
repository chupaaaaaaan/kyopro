{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module My.Data.SegTree where

import Control.Monad.Primitive
import Data.Foldable
import qualified Data.Vector.Unboxing.Mutable as VUM
import qualified Data.Vector.Unboxing as VU
import My.Math.Util
import Control.Monad.Fix
import Data.Bits

data SegTree s e =
    SgT { sizeSgT :: Int
        , segtree :: VUM.MVector s e
        }

type SegTreeIO = SegTree (PrimState IO)

sgtInit :: (Monoid e, VUM.Unboxable e, PrimMonad m) => Int -> e -> m (SegTree (PrimState m) e)
sgtInit n v = let size = bitceil n
              in SgT size <$> VUM.replicate (size*2) v

sgtBuild :: (Monoid e, VUM.Unboxable e, PrimMonad m) => VU.Vector e -> m (SegTree (PrimState m) e)
sgtBuild iv = do
    let il = VU.length iv
        size = bitceil il

    mv <- VUM.replicate (size*2) mempty
    for_ [0..il-1] $ \p -> VUM.write mv (size+p) (iv VU.! p)
    for_ [size-1,size-2..1] $ sgtInnerUpdate mv

    return $ SgT size mv

sgtWrite :: (Monoid e, VUM.Unboxable e, PrimMonad m) => SegTree (PrimState m) e -> Int -> e -> m ()
sgtWrite SgT{..} p v = do
    let pos = sizeSgT + p - 1

    VUM.write segtree pos v
    flip fix pos $ \loop i -> if i <= 1 then return () else do
                     let parent = i .>>. 1
                     sgtInnerUpdate segtree parent
                     loop parent

sgtModify :: (Monoid e, VUM.Unboxable e, PrimMonad m) => SegTree (PrimState m) e -> Int -> e -> m ()
sgtModify SgT{..} p v = do
    let pos = sizeSgT + p - 1

    VUM.modify segtree (v<>) pos
    flip fix pos $ \loop i -> if i <= 1 then return () else do
                     let parent = i .>>. 1
                     sgtInnerUpdate segtree parent
                     loop parent

sgtRead :: (Monoid e, VUM.Unboxable e, PrimMonad m) => SegTree (PrimState m) e -> Int -> m e
sgtRead SgT{..} p = VUM.read segtree (sizeSgT + p - 1)

sgtQuery :: (Monoid e, VUM.Unboxable e, PrimMonad m) => SegTree (PrimState m) e -> Int -> Int -> m e
sgtQuery SgT{..} l r = do
    let go iL iR smL smR
            | iL < iR = do
                  vL <- VUM.read segtree iL
                  vR <- VUM.read segtree (iR-1)
                  let smL' = if odd iL then vL <> smL else smL
                      smR' = if odd iR then smR <> vR else smR
                      iL'  = if odd iL then iL+1 else iL
                      iR'  = if odd iR then iR-1 else iR
                  go (iL' .>>. 1) (iR' .>>. 1) smL' smR'
            | otherwise = return $ smL <> smR

    go ((l-1) + sizeSgT) ((r-1) + sizeSgT) mempty mempty

sgtInnerUpdate :: (Monoid e, VUM.Unboxable e, PrimMonad m) => VUM.MVector (PrimState m) e -> Int -> m ()
sgtInnerUpdate tree u = do
    x <- VUM.read tree (u .<<. 1)
    y <- VUM.read tree (u .<<. 1 + 1)
    VUM.write tree u (x <> y)
