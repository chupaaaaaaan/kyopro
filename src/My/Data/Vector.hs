module My.Data.Vector where

import Data.Vector.Unboxed qualified as VU
import Data.Vector.Algorithms.Intro
import My.Algorithm.BinarySearch


{-# INLINE vSort #-}
vSort :: (Ord a, VU.Unbox a) => VU.Vector a -> VU.Vector a
vSort = VU.modify sort

{-# INLINE vSortBy #-}
vSortBy :: (Ord a, VU.Unbox a) => Comparison a -> VU.Vector a -> VU.Vector a
vSortBy f = VU.modify $ sortBy f

{-# INLINE vSortUniq #-}
vSortUniq :: (Ord a, VU.Unbox a) => VU.Vector a -> VU.Vector a
vSortUniq = vSortUniqBy compare

{-# INLINE vSortUniqBy #-}
vSortUniqBy :: (Ord a, VU.Unbox a) => Comparison a -> VU.Vector a -> VU.Vector a
vSortUniqBy f v = VU.create $ do
    mv <- VU.thaw v
    x <- sortUniqBy f mv
    return x

{-# INLINE vDiff #-}
vDiff :: (Ord a, VU.Unbox a) => VU.Vector a -> VU.Vector a -> VU.Vector a
vDiff v u = let su = vSortUniq u
                ulen = VU.length su
                p x = let i = bsearch (condLE su x) (-1) ulen
                      in i == (-1) || i == ulen || su VU.! i /= x
            in VU.filter p v

